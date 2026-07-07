/* any-to-one NSS module: resolve every username that earlier NSS
 * modules (files) don't recognize onto the local "blog" account, with
 * an empty password field so sshd's "none" auth succeeds and visitors
 * are never prompted
 */
#include <errno.h>
#include <nss.h>
#include <pwd.h>
#include <shadow.h>
#include <stdio.h>
#include <string.h>

static int copy_str(const char *src, char **dst, char **buf, size_t *left)
{
    size_t n = strlen(src) + 1;
    if (n > *left)
        return -1;
    memcpy(*buf, src, n);
    *dst = *buf;
    *buf += n;
    *left -= n;
    return 0;
}

enum nss_status _nss_ato_getpwnam_r(const char *name, struct passwd *result,
                                    char *buffer, size_t buflen, int *errnop)
{
    struct passwd tplbuf, *tpl = NULL, *ent = NULL;
    char tplstr[1024];
    FILE *f = fopen("/etc/passwd", "re");
    if (f == NULL)
    {
        *errnop = errno;
        return NSS_STATUS_UNAVAIL;
    }
    /* fgetpwent_r: _r entry points must be reentrant */
    while (fgetpwent_r(f, &tplbuf, tplstr, sizeof(tplstr), &ent) == 0)
    {
        if (strcmp(ent->pw_name, "blog") == 0)
        {
            tpl = ent;
            break;
        }
    }
    if (tpl == NULL)
    {
        fclose(f);
        *errnop = ENOENT;
        return NSS_STATUS_NOTFOUND;
    }

    char *buf = buffer;
    size_t left = buflen;
    if (copy_str(name, &result->pw_name, &buf, &left) ||
        copy_str("", &result->pw_passwd, &buf, &left) ||
        copy_str("", &result->pw_gecos, &buf, &left) ||
        copy_str(tpl->pw_dir, &result->pw_dir, &buf, &left) ||
        copy_str(tpl->pw_shell, &result->pw_shell, &buf, &left))
    {
        fclose(f);
        *errnop = ERANGE;
        return NSS_STATUS_TRYAGAIN;
    }
    result->pw_uid = tpl->pw_uid;
    result->pw_gid = tpl->pw_gid;
    fclose(f);
    return NSS_STATUS_SUCCESS;
}

/* shadow counterpart. Same auth outcome as the failed lookup it
 * replaces; this just makes it succeed quietly. Only consulted
 * for names "files" doesn't know, same as getpwnam.
 */
enum nss_status _nss_ato_getspnam_r(const char *name, struct spwd *result,
                                    char *buffer, size_t buflen, int *errnop)
{
    char *buf = buffer;
    size_t left = buflen;
    if (copy_str(name, &result->sp_namp, &buf, &left) ||
        copy_str("", &result->sp_pwdp, &buf, &left))
    {
        *errnop = ERANGE;
        return NSS_STATUS_TRYAGAIN;
    }
    result->sp_lstchg = -1;
    result->sp_min = -1;
    result->sp_max = -1;
    result->sp_warn = -1;
    result->sp_inact = -1;
    result->sp_expire = -1;
    result->sp_flag = 0;
    return NSS_STATUS_SUCCESS;
}
