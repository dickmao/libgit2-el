#include <stdlib.h>
#include <assert.h>

#include "git2.h"

#ifdef EGIT_DEBUG
#include "egit-debug.h"
#endif

#include "interface.h"
#include "egit-annotated-commit.h"
#include "egit-blame.h"
#include "egit-blob.h"
#include "egit-branch.h"
#include "egit-checkout.h"
#include "egit-cherrypick.h"
#include "egit-clone.h"
#include "egit-commit.h"
#include "egit-config.h"
#include "egit-cred.h"
#include "egit-describe.h"
#include "egit-diff.h"
#include "egit-graph.h"
#include "egit-ignore.h"
#include "egit-index.h"
#include "egit-libgit2.h"
#include "egit-merge.h"
#include "egit-message.h"
#include "egit-object.h"
#include "egit-pathspec.h"
#include "egit-reference.h"
#include "egit-reflog.h"
#include "egit-refspec.h"
#include "egit-remote.h"
#include "egit-repository.h"
#include "egit-reset.h"
#include "egit-revparse.h"
#include "egit-revert.h"
#include "egit-revwalk.h"
#include "egit-signature.h"
#include "egit-status.h"
#include "egit-submodule.h"
#include "egit-tag.h"
#include "egit-transaction.h"
#include "egit-tree.h"
#include "egit-treebuilder.h"
#include "egit.h"

egit_type egit_get_type(emacs_env *env, emacs_value _obj)
{
    if (!em_user_ptrp(env, _obj))
        return EGIT_UNKNOWN;
    egit_object *obj = (egit_object*) EM_EXTRACT_USER_PTR(_obj);
    return obj->type;
}

bool egit_assert_type(emacs_env *env, emacs_value obj, egit_type type, emacs_value predicate)
{
    if (type == egit_get_type(env, obj))
        return true;
    em_signal_wrong_type(env, predicate, obj);
    return false;
}

bool egit_assert_object(emacs_env *env, emacs_value obj)
{
    egit_type type = egit_get_type(env, obj);
    if (type == EGIT_COMMIT || type == EGIT_TREE ||
        type == EGIT_BLOB || type == EGIT_TAG || type == EGIT_OBJECT)
        return true;
    em_signal_wrong_type(env, esym_libgit_object_p, obj);
    return false;
}

ptrdiff_t egit_assert_list(emacs_env *env, egit_type type, emacs_value predicate, emacs_value arg)
{
    ptrdiff_t nelems = 0;

    while (em_consp(env, arg)) {
        emacs_value car = em_car(env, arg);
        if (!egit_assert_type(env, car, type, predicate))
            return -1;
        nelems++;
        arg = em_cdr(env, arg);
    }

    if (EM_EXTRACT_BOOLEAN(arg)) {
        em_signal_wrong_type(env, esym_listp, arg);
        return -1;
    }

    return nelems;
}

void egit_finalize(void* _obj)
{
#ifdef EGIT_DEBUG
    egit_signal_finalize(_obj);
#endif

    // The argument type must be void* to make this function work as an Emacs finalizer
    egit_object *obj = (egit_object*)_obj;
    egit_object *parent = obj->parent;

    // For reference-counted types, decref and possibly abort
    switch (obj->type) {
    case EGIT_BLAME:
    case EGIT_DIFF:
    case EGIT_INDEX:
    case EGIT_REFLOG:
    case EGIT_REMOTE:
    case EGIT_REPOSITORY:
        obj->refcount--;
        if (obj->refcount != 0)
            return;
    default: break;
    }

#ifdef EGIT_DEBUG
    egit_signal_free(_obj);
#endif

    // Free the object based on its type
    // For types that only expose weak pointers to the parent, this should be a no-op
    switch (obj->type) {
    case EGIT_COMMIT: case EGIT_TREE: case EGIT_BLOB: case EGIT_TAG: case EGIT_OBJECT:
        git_object_free(obj->ptr); break;
    case EGIT_BLAME: git_blame_free(obj->ptr); break;
    case EGIT_DIFF: git_diff_free(obj->ptr); break;
    case EGIT_INDEX: git_index_free(obj->ptr); break;
    case EGIT_REFLOG: git_reflog_free(obj->ptr); break;
    case EGIT_REMOTE: git_remote_free(obj->ptr); break;
    case EGIT_REPOSITORY: git_repository_free(obj->ptr); break;
    case EGIT_REFERENCE: git_reference_free(obj->ptr); break;
    case EGIT_CONFIG: git_config_free(obj->ptr); break;
    case EGIT_SIGNATURE: git_signature_free(obj->ptr); break;
    case EGIT_TRANSACTION: git_transaction_free(obj->ptr); break;
    case EGIT_SUBMODULE: git_submodule_free(obj->ptr); break;
    case EGIT_CRED: git_cred_free(obj->ptr); break;
    case EGIT_ANNOTATED_COMMIT: git_annotated_commit_free(obj->ptr); break;
    case EGIT_REVWALK: git_revwalk_free(obj->ptr); break;
    case EGIT_TREEBUILDER: git_treebuilder_free(obj->ptr); break;
    case EGIT_PATHSPEC: git_pathspec_free(obj->ptr); break;
    case EGIT_PATHSPEC_MATCH_LIST: git_pathspec_match_list_free(obj->ptr); break;
    default: break;
    }

    // Free the wrapper, then release the reference to the parent, if applicable
    free(obj);
    if (parent)
        egit_finalize(parent);
}

emacs_value egit_wrap(emacs_env *env, egit_type type, const void* data, egit_object *parent)
{
    // If it's a git_object, try to be more specific
    if (type == EGIT_OBJECT) {
        switch (git_object_type(data)) {
        case GIT_OBJ_COMMIT: type = EGIT_COMMIT; break;
        case GIT_OBJ_TREE: type = EGIT_TREE; break;
        case GIT_OBJ_BLOB: type = EGIT_BLOB; break;
        case GIT_OBJ_TAG: type = EGIT_TAG; break;
        default: break;
        }
    }

    // Increase refcounts of owner object(s), if applicable
    if (parent)
        parent->refcount++;

    egit_object *wrapper;
    wrapper = (egit_object*) malloc(sizeof(egit_object));
    wrapper->type = type;
    wrapper->ptr = (void*) data;
    wrapper->parent = parent;

    // This has no effect for types that are not reference-counted
    wrapper->refcount = 1;

#ifdef EGIT_DEBUG
    egit_signal_alloc(wrapper);
#endif

    return EM_USER_PTR(wrapper, egit_finalize);
}

typedef emacs_value (*func_0)(emacs_env*);
typedef emacs_value (*func_1)(emacs_env*, emacs_value);
typedef emacs_value (*func_2)(emacs_env*, emacs_value, emacs_value);
typedef emacs_value (*func_3)(emacs_env*, emacs_value, emacs_value, emacs_value);
typedef emacs_value (*func_4)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value);
typedef emacs_value (*func_5)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value, emacs_value);
typedef emacs_value (*func_6)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value, emacs_value, emacs_value);
typedef emacs_value (*func_7)(emacs_env*, emacs_value, emacs_value, emacs_value,
                              emacs_value, emacs_value, emacs_value, emacs_value);

// Get an argument index, or nil. Useful for simulating optional arguments.
#define GET_SAFE(arglist, nargs, index) ((index) < (nargs) ? (arglist)[(index)] : esym_nil)

static emacs_value egit_dispatch_0(emacs_env *env, __attribute__((unused)) ptrdiff_t nargs,
                            __attribute__((unused)) emacs_value *args, void *data)
{
    func_0 func = (func_0) data;
    return func(env);
}

static emacs_value egit_dispatch_1(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_1 func = (func_1) data;
    return func(env, GET_SAFE(args, nargs, 0));
}

static emacs_value egit_dispatch_2(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_2 func = (func_2) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1));
}

static emacs_value egit_dispatch_3(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_3 func = (func_3) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2));
}

static emacs_value egit_dispatch_4(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_4 func = (func_4) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2),
                GET_SAFE(args, nargs, 3));
}

static emacs_value egit_dispatch_5(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_5 func = (func_5) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2),
                GET_SAFE(args, nargs, 3), GET_SAFE(args, nargs, 4));
}

static emacs_value egit_dispatch_6(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_6 func = (func_6) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2),
                GET_SAFE(args, nargs, 3), GET_SAFE(args, nargs, 4), GET_SAFE(args, nargs, 5));
}

static emacs_value egit_dispatch_7(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
    func_7 func = (func_7) data;
    return func(env, GET_SAFE(args, nargs, 0), GET_SAFE(args, nargs, 1), GET_SAFE(args, nargs, 2),
                GET_SAFE(args, nargs, 3), GET_SAFE(args, nargs, 4), GET_SAFE(args, nargs, 5),
                GET_SAFE(args, nargs, 6));
}

bool egit_dispatch_error(emacs_env *env, int retval)
{
    if (retval >= 0) return false;

    const git_error *err = giterr_last();
    if (!err) return false;

    emacs_value error = em_findenum_error(err->klass);
    if (!EM_EXTRACT_BOOLEAN(error))
        error = esym_giterr;

    em_signal(env, error, err->message);
    return true;
}

EGIT_DOC(typeof, "OBJ", "Return the type of the git pointer OBJ, or nil.");
static emacs_value egit_typeof(emacs_env *env, emacs_value val)
{
    switch (egit_get_type(env, val)) {
    case EGIT_REPOSITORY: return esym_repository;
    case EGIT_REFERENCE: return esym_reference;
    case EGIT_COMMIT: return esym_commit;
    case EGIT_TREE: return esym_tree;
    case EGIT_BLOB: return esym_blob;
    case EGIT_TAG: return esym_tag;
    case EGIT_OBJECT: return esym_object;
    case EGIT_BLAME: return esym_blame;
    case EGIT_BLAME_HUNK: return esym_blame_hunk;
    case EGIT_CONFIG: return esym_config;
    case EGIT_TRANSACTION: return esym_transaction;
    case EGIT_INDEX: return esym_index;
    case EGIT_INDEX_ENTRY: return esym_index_entry;
    case EGIT_DIFF: return esym_diff;
    case EGIT_DIFF_DELTA: return esym_diff_delta;
    case EGIT_DIFF_BINARY: return esym_diff_binary;
    case EGIT_DIFF_HUNK: return esym_diff_hunk;
    case EGIT_DIFF_LINE: return esym_diff_line;
    case EGIT_PATHSPEC: return esym_pathspec;
    case EGIT_PATHSPEC_MATCH_LIST: return esym_pathspec_match_list;
    case EGIT_REMOTE: return esym_remote;
    case EGIT_REFSPEC: return esym_refspec;
    case EGIT_SUBMODULE: return esym_submodule;
    case EGIT_CRED: return esym_cred;
    case EGIT_ANNOTATED_COMMIT: return esym_annotated_commit;
    case EGIT_REFLOG: return esym_reflog;
    case EGIT_REFLOG_ENTRY: return esym_reflog_entry;
    case EGIT_REVWALK: return esym_revwalk;
    case EGIT_TREEBUILDER: return esym_treebuilder;
    default: return esym_nil;
    }
}

#define TYPECHECKER(caps, small, text)                                  \
    EGIT_DOC(small##_p, "OBJ", "Return non-nil if OBJ is a git " text "."); \
    static emacs_value egit_##small##_p(emacs_env *env, emacs_value obj)\
    {                                                                   \
        return egit_get_type(env, obj) == EGIT_##caps ? esym_t : esym_nil;  \
    }

TYPECHECKER(ANNOTATED_COMMIT, annotated_commit, "annotated commit");
TYPECHECKER(BLAME, blame, "blame");
TYPECHECKER(BLAME_HUNK, blame_hunk, "blame hunk");
TYPECHECKER(COMMIT, commit, "commit");
TYPECHECKER(BLOB, blob, "blob");
TYPECHECKER(CONFIG, config, "config");
TYPECHECKER(CRED, cred, "credential");
TYPECHECKER(DIFF, diff, "diff");
TYPECHECKER(DIFF_DELTA, diff_delta, "diff delta");
TYPECHECKER(DIFF_BINARY, diff_binary, "diff binary");
TYPECHECKER(DIFF_HUNK, diff_hunk, "diff hunk");
TYPECHECKER(DIFF_LINE, diff_line, "diff line");
TYPECHECKER(INDEX, index, "index.");
TYPECHECKER(INDEX_ENTRY, index_entry, "index entry");
TYPECHECKER(PATHSPEC, pathspec, "pathspec");
TYPECHECKER(PATHSPEC_MATCH_LIST, pathspec_match_list, "pathspec match list");
TYPECHECKER(REFERENCE, reference, "reference");
TYPECHECKER(REFLOG, reflog, "reflog");
TYPECHECKER(REFLOG_ENTRY, reflog_entry, "reflog entry");
TYPECHECKER(REFSPEC, refspec, "refspec");
TYPECHECKER(REMOTE, remote, "remote");
TYPECHECKER(REPOSITORY, repository, "repository");
TYPECHECKER(REVWALK, revwalk, "repository");
TYPECHECKER(SIGNATURE, signature, "signature");
TYPECHECKER(SUBMODULE, submodule, "submodule");
TYPECHECKER(TAG, tag, "tag");
TYPECHECKER(TRANSACTION, transaction, "transaction");
TYPECHECKER(TREE, tree, "tree");
TYPECHECKER(TREEBUILDER, treebuilder, "treebuilder");

#undef TYPECHECKER

EGIT_DOC(object_p, "OBJ", "Return non-nil if OBJ is a git object.");
static emacs_value egit_object_p(emacs_env *env, emacs_value obj)
{
    egit_type type = egit_get_type(env, obj);
    return (type == EGIT_COMMIT || type == EGIT_TREE || type == EGIT_BLOB ||
            type == EGIT_TAG || type == EGIT_OBJECT) ? esym_t : esym_nil;
}

#define DEFUN(ename, cname, min_nargs, max_nargs)                       \
    em_defun(env, (ename),                                              \
             env->make_function(                                        \
                 env, (min_nargs), (max_nargs),                         \
                 egit_dispatch_##max_nargs,                             \
                 egit_##cname##__doc,                                   \
                 egit_##cname))

void egit_init(emacs_env *env)
{
    // Debug mode functions
#ifdef EGIT_DEBUG
    DEFUN("libgit2--allocs", _allocs, 0, 0);
    DEFUN("libgit2--finalizes", _finalizes, 0, 0);
    DEFUN("libgit2--frees", _frees, 0, 0);
    DEFUN("libgit2--refcount", _refcount, 1, 1);
    DEFUN("libgit2--wrapper", _wrapper, 1, 1);
    DEFUN("libgit2--wrapped", _wrapped, 1, 1);
    DEFUN("libgit2--parent-wrapper", _parent_wrapper, 1, 1);
#endif

    // Type checkers
    DEFUN("libgit2-typeof", typeof, 1, 1);
    DEFUN("libgit2-annotated-commit-p", annotated_commit_p, 1, 1);
    DEFUN("libgit2-blame-p", blame_p, 1, 1);
    DEFUN("libgit2-blame-hunk-p", blame_hunk_p, 1, 1);
    DEFUN("libgit2-blob-p", blob_p, 1, 1);
    DEFUN("libgit2-commit-p", commit_p, 1, 1);
    DEFUN("libgit2-config-p", config_p, 1, 1);
    DEFUN("libgit2-cred-p", cred_p, 1, 1);
    DEFUN("libgit2-diff-p", diff_p, 1, 1);
    DEFUN("libgit2-diff-delta-p", diff_delta_p, 1, 1);
    DEFUN("libgit2-diff-binary-p", diff_binary_p, 1, 1);
    DEFUN("libgit2-diff-hunk-p", diff_hunk_p, 1, 1);
    DEFUN("libgit2-diff-line-p", diff_line_p, 1, 1);
    DEFUN("libgit2-index-p", index_p, 1, 1);
    DEFUN("libgit2-index-entry-p", index_entry_p, 1, 1);
    DEFUN("libgit2-object-p", object_p, 1, 1);
    DEFUN("libgit2-pathspec-p", pathspec_p, 1, 1);
    DEFUN("libgit2-pathspec-match-list-p", pathspec_match_list_p, 1, 1);
    DEFUN("libgit2-reference-p", reference_p, 1, 1);
    DEFUN("libgit2-reflog-p", reflog_p, 1, 1);
    DEFUN("libgit2-reflog-entry-p", reflog_entry_p, 1, 1);
    DEFUN("libgit2-refspec-p", refspec_p, 1, 1);
    DEFUN("libgit2-remote-p", remote_p, 1, 1);
    DEFUN("libgit2-repository-p", repository_p, 1, 1);
    DEFUN("libgit2-revwalk-p", revwalk_p, 1, 1);
    DEFUN("libgit2-signature-p", signature_p, 1, 1);
    DEFUN("libgit2-submodule-p", submodule_p, 1, 1);
    DEFUN("libgit2-tag-p", tag_p, 1, 1);
    DEFUN("libgit2-transaction-p", transaction_p, 1, 1);
    DEFUN("libgit2-tree-p", tree_p, 1, 1);
    DEFUN("libgit2-treebuilder-p", treebuilder_p, 1, 1);

    // Libgit2 (not namespaced as others!)
    DEFUN("libgit2-feature-p", libgit2_feature_p, 1, 1);
    DEFUN("libgit2-version", libgit2_version, 0, 0);

    // Annotated commit
    DEFUN("libgit2-annotated-commit-from-ref", annotated_commit_from_ref, 2, 2);
    DEFUN("libgit2-annotated-commit-from-fetchhead", annotated_commit_from_fetchhead, 4, 4);
    DEFUN("libgit2-annotated-commit-from-revspec", annotated_commit_from_revspec, 2, 2);
    DEFUN("libgit2-annotated-commit-lookup", annotated_commit_lookup, 2, 2);
    DEFUN("libgit2-annotated-commit-id", annotated_commit_id, 1, 1);

    // Blame
    DEFUN("libgit2-blame-file", blame_file, 2, 3);
    DEFUN("libgit2-blame-get-hunk-byindex", blame_get_hunk_byindex, 2, 2);
    DEFUN("libgit2-blame-get-hunk-byline", blame_get_hunk_byline, 2, 2);
    DEFUN("libgit2-blame-get-hunk-count", blame_get_hunk_count, 1, 1);

    DEFUN("libgit2-blame-hunk-commit-id", blame_hunk_commit_id, 1, 2);
    DEFUN("libgit2-blame-hunk-lines", blame_hunk_lines, 1, 1);
    DEFUN("libgit2-blame-hunk-orig-path", blame_hunk_orig_path, 1, 1);
    DEFUN("libgit2-blame-hunk-signature", blame_hunk_signature, 1, 2);
    DEFUN("libgit2-blame-hunk-start-line-number", blame_hunk_start_line_number, 1, 2);

    // Blob
    DEFUN("libgit2-blob-create-fromdisk", blob_create_fromdisk, 2, 2);
    DEFUN("libgit2-blob-create-fromstring", blob_create_fromstring, 2, 2);
    DEFUN("libgit2-blob-create-fromworkdir", blob_create_fromworkdir, 2, 2);
    DEFUN("libgit2-blob-lookup", blob_lookup, 2, 2);
    DEFUN("libgit2-blob-lookup-prefix", blob_lookup_prefix, 2, 2);

    DEFUN("libgit2-blob-binary-p", blob_binary_p, 1, 1);
    DEFUN("libgit2-blob-filtered-content", blob_filtered_content, 2, 3);
    DEFUN("libgit2-blob-id", blob_id, 1, 1);
    DEFUN("libgit2-blob-owner", blob_owner, 1, 1);
    DEFUN("libgit2-blob-rawcontent", blob_rawcontent, 1, 1);
    DEFUN("libgit2-blob-rawsize", blob_rawsize, 1, 1);

    // Branch
    DEFUN("libgit2-branch-create", branch_create, 3, 4);
    DEFUN("libgit2-branch-create-from-annotated", branch_create_from_annotated, 3, 4);
    DEFUN("libgit2-branch-lookup", branch_lookup, 2, 3);
    DEFUN("libgit2-branch-delete", branch_delete, 1, 1);
    DEFUN("libgit2-branch-checked-out-p", branch_checked_out_p, 1, 1);
    DEFUN("libgit2-branch-foreach", branch_foreach, 3, 3);
    DEFUN("libgit2-branch-head-p", branch_head_p, 1, 1);
    DEFUN("libgit2-branch-move", branch_move, 2, 3);
    DEFUN("libgit2-branch-name", branch_name, 1, 1);
    DEFUN("libgit2-branch-remote-name", branch_remote_name, 2, 2);
    DEFUN("libgit2-branch-set-upstream", branch_set_upstream, 2, 2);
    DEFUN("libgit2-branch-upstream", branch_upstream, 1, 1);
    DEFUN("libgit2-branch-upstream-name", branch_upstream_name, 2, 2);
    DEFUN("libgit2-branch-upstream-remote", branch_upstream_remote, 2, 2);

    // Checkout
    DEFUN("libgit2-checkout-head", checkout_head, 1, 2);
    DEFUN("libgit2-checkout-index", checkout_index, 1, 3);
    DEFUN("libgit2-checkout-tree", checkout_tree, 1, 3);

    // Cherrypick
    DEFUN("libgit2-cherrypick", cherrypick, 2, 5);
    DEFUN("libgit2-cherrypick-commit", cherrypick_commit, 3, 5);

    // Clone
    DEFUN("libgit2-clone", clone, 2, 2);

    // Commit
    DEFUN("libgit2-commit-lookup", commit_lookup, 2, 2);
    DEFUN("libgit2-commit-lookup-prefix", commit_lookup_prefix, 2, 2);

    DEFUN("libgit2-commit-author", commit_author, 1, 1);
    DEFUN("libgit2-commit-body", commit_body, 1, 1);
    DEFUN("libgit2-commit-committer", commit_committer, 1, 1);
    DEFUN("libgit2-commit-id", commit_id, 1, 1);
    DEFUN("libgit2-commit-message", commit_message, 1, 1);
    DEFUN("libgit2-commit-nth-gen-ancestor", commit_nth_gen_ancestor, 2, 2);
    DEFUN("libgit2-commit-owner", commit_owner, 1, 1);
    DEFUN("libgit2-commit-parent", commit_parent, 1, 2);
    DEFUN("libgit2-commit-parent-id", commit_parent_id, 1, 2);
    DEFUN("libgit2-commit-parentcount", commit_parentcount, 1, 1);
    DEFUN("libgit2-commit-summary", commit_summary, 1, 1);
    DEFUN("libgit2-commit-time", commit_time, 1, 1);
    DEFUN("libgit2-commit-tree", commit_tree, 1, 1);
    DEFUN("libgit2-commit-tree-id", commit_tree_id, 1, 1);

    DEFUN("libgit2-commit-create", commit_create, 6, 7);

    // Config
    DEFUN("libgit2-config-new", config_new, 0, 0);
    DEFUN("libgit2-config-open-default", config_open_default, 0, 0);
    DEFUN("libgit2-config-open-global", config_open_global, 1, 1);
    DEFUN("libgit2-config-open-level", config_open_level, 1, 2);
    DEFUN("libgit2-config-open-ondisk", config_open_ondisk, 1, 1);
    DEFUN("libgit2-config-snapshot", config_snapshot, 1, 1);

    DEFUN("libgit2-config-get-bool", config_get_bool, 2, 2);
    DEFUN("libgit2-config-get-int", config_get_int, 2, 2);
    DEFUN("libgit2-config-get-path", config_get_path, 2, 2);
    DEFUN("libgit2-config-get-string", config_get_string, 2, 2);
    DEFUN("libgit2-config-lock", config_lock, 1, 1);

    DEFUN("libgit2-config-set-bool", config_set_bool, 3, 3);
    DEFUN("libgit2-config-set-int", config_set_int, 3, 3);
    DEFUN("libgit2-config-set-string", config_set_string, 3, 3);

    DEFUN("libgit2-config-add-file-ondisk", config_add_file_ondisk, 2, 5);
    DEFUN("libgit2-config-delete-entry", config_delete_entry, 2, 2);
    DEFUN("libgit2-config-delete-multivar", config_delete_multivar, 3, 3);

    DEFUN("libgit2-config-find-global", config_find_global, 0, 0);
    DEFUN("libgit2-config-find-programdata", config_find_programdata, 0, 0);
    DEFUN("libgit2-config-find-system", config_find_system, 0, 0);
    DEFUN("libgit2-config-find-xdg", config_find_xdg, 0, 0);

    // Cred
    DEFUN("libgit2-cred-default-new", cred_default_new, 0, 0);
    DEFUN("libgit2-cred-ssh-key-from-agent", cred_ssh_key_from_agent, 1, 1);
    DEFUN("libgit2-cred-ssh-key-memory-new", cred_ssh_key_memory_new, 4, 4);
    DEFUN("libgit2-cred-ssh-key-new", cred_ssh_key_new, 4, 4);
    DEFUN("libgit2-cred-username-new", cred_username_new, 1, 1);
    DEFUN("libgit2-cred-userpass-plaintext-new", cred_userpass_plaintext_new, 2, 2);
    DEFUN("libgit2-cred-username-p", cred_username_p, 1, 1);

    // Describe
    DEFUN("libgit2-describe-commit", describe_commit, 1, 2);
    DEFUN("libgit2-describe-workdir", describe_workdir, 1, 2);

    // Diff
    DEFUN("libgit2-diff-index-to-index", diff_index_to_index, 3, 4);
    DEFUN("libgit2-diff-index-to-workdir", diff_index_to_workdir, 1, 3);
    DEFUN("libgit2-diff-tree-to-index", diff_tree_to_index, 1, 4);
    DEFUN("libgit2-diff-tree-to-tree", diff_tree_to_tree, 1, 4);
    DEFUN("libgit2-diff-tree-to-workdir", diff_tree_to_workdir, 1, 3);
    DEFUN("libgit2-diff-tree-to-workdir-with-index", diff_tree_to_workdir_with_index, 1, 3);
    DEFUN("libgit2-diff-find-similar", diff_find_similar, 1, 2);

    DEFUN("libgit2-diff-foreach", diff_foreach, 2, 5);
    DEFUN("libgit2-diff-print", diff_print, 1, 3);

    DEFUN("libgit2-diff-delta-file-id", diff_delta_file_id, 1, 2);
    DEFUN("libgit2-diff-delta-file-path", diff_delta_file_path, 1, 2);
    DEFUN("libgit2-diff-delta-nfiles", diff_delta_nfiles, 1, 1);
    DEFUN("libgit2-diff-delta-similarity", diff_delta_similarity, 1, 1);
    DEFUN("libgit2-diff-delta-status", diff_delta_status, 1, 1);
    DEFUN("libgit2-diff-delta-file-exists-p", diff_delta_file_exists_p, 1, 2);

    DEFUN("libgit2-diff-hunk-header", diff_hunk_header, 1, 1);
    DEFUN("libgit2-diff-hunk-lines", diff_hunk_lines, 1, 2);
    DEFUN("libgit2-diff-hunk-start", diff_hunk_start, 1, 2);

    DEFUN("libgit2-diff-line-origin", diff_line_origin, 1, 1);
    DEFUN("libgit2-diff-line-lineno", diff_line_lineno, 2, 2);
    DEFUN("libgit2-diff-line-content", diff_line_content, 1, 1);

    DEFUN("libgit2-diff-get-delta", diff_get_delta, 2, 2);
    DEFUN("libgit2-diff-num-deltas", diff_num_deltas, 1, 2);

    // Graph
    DEFUN("libgit2-graph-ahead-behind", graph_ahead_behind, 3, 3);
    DEFUN("libgit2-graph-descendant-p", graph_descendant_p, 3, 3);

    // Ignore
    DEFUN("libgit2-ignore-add-rule", add_rule, 2, 2);
    DEFUN("libgit2-ignore-clear-internal-rules", clear_internal_rules, 1, 1);
    DEFUN("libgit2-ignore-path-ignored-p", path_ignored_p, 2, 2);

    // Index
    DEFUN("libgit2-index-caps", index_caps, 1, 1);
    DEFUN("libgit2-index-checksum", index_checksum, 1, 1);
    DEFUN("libgit2-index-conflict-foreach", index_conflict_foreach, 2, 2);
    DEFUN("libgit2-index-conflict-get", index_conflict_get, 2, 2);
    DEFUN("libgit2-index-entry-id", index_entry_id, 1, 1);
    DEFUN("libgit2-index-entry-path", index_entry_path, 1, 1);
    DEFUN("libgit2-index-entry-stage", index_entry_stage, 1, 1);
    DEFUN("libgit2-index-entrycount", index_entrycount, 1, 1);
    DEFUN("libgit2-index-get-byindex", index_get_byindex, 2, 2);
    DEFUN("libgit2-index-get-bypath", index_get_bypath, 2, 3);
    DEFUN("libgit2-index-owner", index_owner, 1, 1);
    DEFUN("libgit2-index-path", index_path, 1, 1);
    DEFUN("libgit2-index-version", index_version, 1, 1);
    DEFUN("libgit2-index-conflicts-p", index_conflicts_p, 1, 1);

    DEFUN("libgit2-index-add-all", index_add_all, 1, 4);
    DEFUN("libgit2-index-add-bypath", index_add_bypath, 2, 2);
    DEFUN("libgit2-index-clear", index_clear, 1, 1);
    DEFUN("libgit2-index-read", index_read, 1, 2);
    DEFUN("libgit2-index-write", index_write, 1, 1);
    DEFUN("libgit2-index-write-tree", index_write_tree, 1, 2);

    // Merge
    DEFUN("libgit2-merge", merge, 2, 4);
    DEFUN("libgit2-merge-analysis", merge_analysis, 2, 2);
    DEFUN("libgit2-merge-base", merge_base, 2, 2);
    DEFUN("libgit2-merge-base-octopus", merge_base_octopus, 2, 2);
    DEFUN("libgit2-merge-bases", merge_bases, 2, 2);

    // Message
    DEFUN("libgit2-message-prettify", message_prettify, 1, 2);
    DEFUN("libgit2-message-trailers", message_trailers, 1, 1);

    // Object
    DEFUN("libgit2-object-lookup", object_lookup, 2, 3);
    DEFUN("libgit2-object-lookup-prefix", object_id, 2, 3);

    DEFUN("libgit2-object-id", object_id, 1, 1);
    DEFUN("libgit2-object-owner", object_owner, 1, 1);
    DEFUN("libgit2-object-short-id", object_short_id, 1, 1);

    // Pathspec
    DEFUN("libgit2-pathspec-new", pathspec_new, 1, 1);
    DEFUN("libgit2-pathspec-matches-path", pathspec_matches_path, 3, 3);
    DEFUN("libgit2-pathspec-match-list-entrycount", pathspec_match_list_entrycount, 1, 1);
    DEFUN("libgit2-pathspec-match-list-entry", pathspec_match_list_entry, 2, 2);
    DEFUN("libgit2-pathspec-match-list-diff-entry", pathspec_match_list_diff_entry, 2, 2);
    DEFUN("libgit2-pathspec-match-list-failed-entrycount", pathspec_match_list_failed_entrycount, 1, 1);
    DEFUN("libgit2-pathspec-match-list-failed-entry", pathspec_match_list_failed_entry, 2, 2);
    DEFUN("libgit2-pathspec-match-workdir", pathspec_match_workdir, 3, 3);
    DEFUN("libgit2-pathspec-match-index", pathspec_match_index, 3, 3);
    DEFUN("libgit2-pathspec-match-tree", pathspec_match_tree, 3, 3);
    DEFUN("libgit2-pathspec-match-diff", pathspec_match_diff, 3, 3);

    // Reference
    DEFUN("libgit2-reference-create", reference_create, 3, 5);
    DEFUN("libgit2-reference-create-matching", reference_create_matching, 3, 6);
    DEFUN("libgit2-reference-dup", reference_dup, 1, 1);
    DEFUN("libgit2-reference-dwim", reference_dwim, 2, 2);
    DEFUN("libgit2-reference-lookup", reference_lookup, 2, 2);

    DEFUN("libgit2-reference-list", reference_list, 1, 1);
    DEFUN("libgit2-reference-name", reference_name, 1, 1);
    DEFUN("libgit2-reference-owner", reference_owner, 1, 1);
    DEFUN("libgit2-reference-peel", reference_peel, 1, 2);
    DEFUN("libgit2-reference-resolve", reference_resolve, 1, 1);
    DEFUN("libgit2-reference-shorthand", reference_shorthand, 1, 1);
    DEFUN("libgit2-reference-symbolic-target", reference_symbolic_target, 1, 1);
    DEFUN("libgit2-reference-target", reference_target, 1, 1);
    DEFUN("libgit2-reference-target-peel", reference_target, 1, 1);
    DEFUN("libgit2-reference-type", reference_type, 1, 1);

    DEFUN("libgit2-reference-delete", reference_delete, 1, 1);
    DEFUN("libgit2-reference-ensure-log", reference_ensure_log, 2, 2);
    DEFUN("libgit2-reference-remove", reference_delete, 2, 2);

    DEFUN("libgit2-reference-branch-p", reference_branch_p, 1, 1);
    DEFUN("libgit2-reference-direct-p", reference_direct_p, 1, 1);
    DEFUN("libgit2-reference-has-log-p", reference_has_log_p, 2, 2);
    DEFUN("libgit2-reference-name-to-id", reference_name_to_id, 2, 2);
    DEFUN("libgit2-reference-note-p", reference_note_p, 1, 1);
    DEFUN("libgit2-reference-remote-p", reference_remote_p, 1, 1);
    DEFUN("libgit2-reference-symbolic-p", reference_symbolic_p, 1, 1);
    DEFUN("libgit2-reference-tag-p", reference_tag_p, 1, 1);
    DEFUN("libgit2-reference-valid-name-p", reference_valid_name_p, 1, 1);

    DEFUN("libgit2-reference-foreach", reference_foreach, 2, 2);
    DEFUN("libgit2-reference-foreach-glob", reference_foreach_glob, 3, 3);
    DEFUN("libgit2-reference-foreach-name", reference_foreach_name, 2, 2);

    // Reflog
    DEFUN("libgit2-reflog-read", reflog_read, 1, 2);
    DEFUN("libgit2-reflog-entry-byindex", reflog_entry_byindex, 2, 2);
    DEFUN("libgit2-reflog-entry-committer", reflog_entry_committer, 1, 1);
    DEFUN("libgit2-reflog-entry-id", reflog_entry_id, 1, 2);
    DEFUN("libgit2-reflog-entry-message", reflog_entry_message, 1, 1);
    DEFUN("libgit2-reflog-entrycount", reflog_entrycount, 1, 1);
    DEFUN("libgit2-reflog-append", reflog_append, 3, 4);
    DEFUN("libgit2-reflog-drop", reflog_drop, 2, 3);
    DEFUN("libgit2-reflog-rename", reflog_rename, 3, 3);
    DEFUN("libgit2-reflog-write", reflog_write, 1, 1);

    // Refspec
    DEFUN("libgit2-refspec-direction", refspec_direction, 1, 1);
    DEFUN("libgit2-refspec-dst", refspec_dst, 1, 1);
    DEFUN("libgit2-refspec-src", refspec_src, 1, 1);
    DEFUN("libgit2-refspec-string", refspec_string, 1, 1);
    DEFUN("libgit2-refspec-dst-matches-p", refspec_dst_matches_p, 2, 2);
    DEFUN("libgit2-refspec-force-p", refspec_force_p, 1, 1);
    DEFUN("libgit2-refspec-src-matches-p", refspec_src_matches_p, 2, 2);

    // Remote
    DEFUN("libgit2-remote-create", remote_create, 3, 3);
    DEFUN("libgit2-remote-lookup", remote_lookup, 2, 2);

    DEFUN("libgit2-remote-autotag", remote_autotag, 1, 1);
    DEFUN("libgit2-remote-get-refspec", remote_get_refspec, 2, 2);
    DEFUN("libgit2-remote-get-refspecs", remote_get_refspecs, 1, 2);
    DEFUN("libgit2-remote-name", remote_name, 1, 1);
    DEFUN("libgit2-remote-owner", remote_owner, 1, 1);
    DEFUN("libgit2-remote-pushurl", remote_pushurl, 1, 1);
    DEFUN("libgit2-remote-refspec-count", remote_refspec_count, 1, 1);
    DEFUN("libgit2-remote-url", remote_url, 1, 1);
    DEFUN("libgit2-remote-list", remote_list, 1, 1);
    DEFUN("libgit2-remote-valid-name-p", remote_valid_name_p, 1, 1);

    DEFUN("libgit2-remote-add-refspec", remote_add_refspec, 3, 4);
    DEFUN("libgit2-remote-fetch", remote_fetch, 1, 4);
    DEFUN("libgit2-remote-push", remote_push, 1, 3);

    // Repository
    DEFUN("libgit2-repository-init", repository_init, 1, 2);
    DEFUN("libgit2-repository-open", repository_open, 1, 1);
    DEFUN("libgit2-repository-open-bare", repository_open_bare, 1, 1);

    DEFUN("libgit2-repository-commondir", repository_commondir, 1, 1);
    DEFUN("libgit2-repository-config", repository_config, 1, 1);
    DEFUN("libgit2-repository-get-namespace", repository_get_namespace, 1, 1);
    DEFUN("libgit2-repository-head", repository_head, 1, 1);
    DEFUN("libgit2-repository-head-for-worktree", repository_head_for_worktree, 2, 2);
    DEFUN("libgit2-repository-ident", repository_ident, 1, 1);
    DEFUN("libgit2-repository-index", repository_index, 1, 1);
    DEFUN("libgit2-repository-message", repository_message, 1, 1);
    DEFUN("libgit2-repository-path", repository_path, 1, 1);
    DEFUN("libgit2-repository-state", repository_state, 1, 1);
    DEFUN("libgit2-repository-workdir", repository_workdir, 1, 1);

    DEFUN("libgit2-repository-detach-head", repository_detach_head, 1, 1);
    DEFUN("libgit2-repository-message-remove", repository_message_remove, 1, 1);
    DEFUN("libgit2-repository-set-head", repository_set_head, 2, 2);
    DEFUN("libgit2-repository-set-head-detached", repository_set_head_detached, 2, 2);
    DEFUN("libgit2-repository-set-ident", repository_set_ident, 1, 3);
    DEFUN("libgit2-repository-set-namespace", repository_set_namespace, 2, 2);
    DEFUN("libgit2-repository-set-workdir", repository_set_workdir, 2, 3);
    DEFUN("libgit2-repository-state-cleanup", repository_state_cleanup, 1, 1);

    DEFUN("libgit2-repository-bare-p", repository_bare_p, 1, 1);
    DEFUN("libgit2-repository-empty-p", repository_empty_p, 1, 1);
    DEFUN("libgit2-repository-head-detached-p", repository_empty_p, 1, 1);
    DEFUN("libgit2-repository-head-unborn-p", repository_empty_p, 1, 1);
    DEFUN("libgit2-repository-shallow-p", repository_shallow_p, 1, 1);
    DEFUN("libgit2-repository-worktree-p", repository_worktree_p, 1, 1);

    DEFUN("libgit2-repository-discover", repository_discover, 0, 3);

    // Reset
    DEFUN("libgit2-reset", reset, 3, 4);
    DEFUN("libgit2-reset-from-annotated", reset_from_annotated, 3, 4);
    DEFUN("libgit2-reset-default", reset_default, 3, 3);

    // Revert
    DEFUN("libgit2-revert", revert, 2, 5);
    DEFUN("libgit2-revert-commit", revert_commit, 3, 5);

    // Revparse
    DEFUN("libgit2-revparse", revparse, 2, 2);
    DEFUN("libgit2-revparse-ext", revparse_ext, 2, 2);
    DEFUN("libgit2-revparse-single", revparse_single, 2, 2);

    // Revwalk
    DEFUN("libgit2-revwalk-new", revwalk_new, 1, 1);
    DEFUN("libgit2-revwalk-repository", revwalk_repository, 1, 1);

    DEFUN("libgit2-revwalk-hide", revwalk_hide, 2, 2);
    DEFUN("libgit2-revwalk-hide-glob", revwalk_hide_glob, 2, 2);
    DEFUN("libgit2-revwalk-hide-head", revwalk_hide_head, 1, 1);
    DEFUN("libgit2-revwalk-hide-ref", revwalk_hide_ref, 2, 2);

    DEFUN("libgit2-revwalk-push", revwalk_push, 2, 2);
    DEFUN("libgit2-revwalk-push-glob", revwalk_push_glob, 2, 2);
    DEFUN("libgit2-revwalk-push-head", revwalk_push_head, 1, 1);
    DEFUN("libgit2-revwalk-push-range", revwalk_push_range, 2, 2);
    DEFUN("libgit2-revwalk-push-ref", revwalk_push_ref, 2, 2);

    DEFUN("libgit2-revwalk-reset", revwalk_reset, 1, 1);
    DEFUN("libgit2-revwalk-simplifiy-first-parent", revwalk_simplify_first_parent, 1, 1);
    DEFUN("libgit2-revwalk-sorting", revwalk_sorting, 1, 2);

    DEFUN("libgit2-revwalk-foreach", revwalk_foreach, 2, 3);

    // Signature
    DEFUN("libgit2-signature-default", signature_default, 1, 1);
    DEFUN("libgit2-signature-from-string", signature_from_string, 1, 1);
    DEFUN("libgit2-signature-new", signature_new, 3, 3);
    DEFUN("libgit2-signature-now", signature_now, 2, 2);
    DEFUN("libgit2-signature-name", signature_name, 1, 1);
    DEFUN("libgit2-signature-email", signature_email, 1, 1);
    DEFUN("libgit2-signature-time", signature_time, 1, 1);

    // Status
    DEFUN("libgit2-status-decode", status_decode, 1, 1);
    DEFUN("libgit2-status-file", status_file, 2, 2);
    DEFUN("libgit2-status-should-ignore-p", status_should_ignore_p, 2, 2);
    DEFUN("libgit2-status-foreach-ext", status_foreach_ext, 2, 6);

    // Submodule
    DEFUN("libgit2-submodule-add-setup", submodule_add_setup, 3, 4);
    DEFUN("libgit2-submodule-lookup", submodule_lookup, 2, 2);
    DEFUN("libgit2-submodule-foreach", submodule_foreach, 2, 2);

    DEFUN("libgit2-submodule-branch", submodule_branch, 1, 1);
    DEFUN("libgit2-submodule-fetch-recurse-submodules", submodule_fetch_recurse_submodules, 1, 1);
    DEFUN("libgit2-submodule-head-id", submodule_head_id, 1, 1);
    DEFUN("libgit2-submodule-ignore", submodule_ignore, 1, 1);
    DEFUN("libgit2-submodule-index-id", submodule_index_id, 1, 1);
    DEFUN("libgit2-submodule-location", submodule_location, 1, 2);
    DEFUN("libgit2-submodule-name", submodule_name, 1, 1);
    DEFUN("libgit2-submodule-open", submodule_open, 1, 1);
    DEFUN("libgit2-submodule-owner", submodule_owner, 1, 1);
    DEFUN("libgit2-submodule-path", submodule_path, 1, 1);
    DEFUN("libgit2-submodule-status", submodule_status, 2, 4);
    DEFUN("libgit2-submodule-update-strategy", submodule_update_strategy, 1, 1);
    DEFUN("libgit2-submodule-url", submodule_url, 1, 1);
    DEFUN("libgit2-submodule-wd-id", submodule_wd_id, 1, 1);

    DEFUN("libgit2-submodule-add-finalize", submodule_add_finalize, 1, 1);
    DEFUN("libgit2-submodule-add-to-index", submodule_add_to_index, 1, 2);
    DEFUN("libgit2-submodule-init", submodule_init, 1, 2);
    DEFUN("libgit2-submodule-reload", submodule_reload, 1, 2);
    DEFUN("libgit2-submodule-repo-init", submodule_repo_init, 1, 2);
    DEFUN("libgit2-submodule-set-branch", submodule_set_branch, 3, 3);
    DEFUN("libgit2-submodule-set-fetch-recurse-submodules", submodule_set_fetch_recurse_submodules, 2, 3);
    DEFUN("libgit2-submodule-set-ignore", submodule_set_ignore, 3, 3);
    DEFUN("libgit2-submodule-set-update", submodule_set_update, 3, 3);
    DEFUN("libgit2-submodule-set-url", submodule_set_url, 3, 3);
    DEFUN("libgit2-submodule-sync", submodule_sync, 1, 1);
    DEFUN("libgit2-submodule-update", submodule_sync, 1, 5);

    // Tag
    DEFUN("libgit2-tag-lookup", tag_lookup, 2, 2);
    DEFUN("libgit2-tag-lookup-prefix", tag_lookup_prefix, 2, 2);
    DEFUN("libgit2-tag-foreach", tag_foreach, 2, 2);
    DEFUN("libgit2-tag-id", tag_id, 1, 1);
    DEFUN("libgit2-tag-owner", tag_owner, 1, 1);
    DEFUN("libgit2-tag-message", tag_message, 1, 1);
    DEFUN("libgit2-tag-name", tag_name, 1, 1);
    DEFUN("libgit2-tag-peel", tag_peel, 1, 1);
    DEFUN("libgit2-tag-target", tag_target, 1, 1);
    DEFUN("libgit2-tag-target-id", tag_target_id, 1, 1);
    DEFUN("libgit2-tag-target-type", tag_target_type, 1, 1);
    DEFUN("libgit2-tag-list", tag_list, 1, 2);

    // Transaction
    DEFUN("libgit2-transaction-commit", transaction_commit, 1, 1);

    // Tree
    DEFUN("libgit2-tree-lookup", tree_lookup, 2, 2);
    DEFUN("libgit2-tree-lookup-prefix", tree_lookup_prefix, 2, 2);

    DEFUN("libgit2-tree-id", tree_id, 1, 1);
    DEFUN("libgit2-tree-owner", tree_owner, 1, 1);

    DEFUN("libgit2-tree-entry-byid", tree_entry_byid, 2, 2);
    DEFUN("libgit2-tree-entry-byindex", tree_entry_byindex, 2, 2);
    DEFUN("libgit2-tree-entry-byname", tree_entry_byname, 2, 2);
    DEFUN("libgit2-tree-entry-bypath", tree_entry_bypath, 2, 2);
    DEFUN("libgit2-tree-entrycount", tree_entrycount, 1, 1);

    DEFUN("libgit2-tree-walk", tree_walk, 3, 3);

    // Treebuilder
    DEFUN("libgit2-treebuilder-new", treebuilder_new, 1, 2);
    DEFUN("libgit2-treebuilder-entrycount", treebuilder_entrycount, 1, 1);
    DEFUN("libgit2-treebuilder-get", treebuilder_get, 2, 2);
    DEFUN("libgit2-treebuilder-clear", treebuilder_clear, 1, 1);
    DEFUN("libgit2-treebuilder-insert", treebuilder_insert, 4, 4);
    DEFUN("libgit2-treebuilder-remove", treebuilder_remove, 2, 2);
    DEFUN("libgit2-treebuilder-write", treebuilder_write, 1, 1);
    DEFUN("libgit2-treebuilder-filter", treebuilder_filter, 2, 2);
}
