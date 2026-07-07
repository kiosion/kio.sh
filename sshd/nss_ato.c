/* any-to-one NSS module: resolve every username that earlier NSS
 * modules (files) don't recognize onto the local "blog" account, with
 * an empty password field so sshd's "none" auth succeeds and visitors
 * are never prompted. Only getpwnam is implemented; uid lookups and
 * enumeration behave normally. The requested name is preserved in
 * pw_name so the TUI can greet the visitor via $USER.
 */
#include <errno.h>
#include <nss.h>
#include <pwd.h>
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
    struct passwd *tpl = NULL;
    FILE *f = fopen("/etc/passwd", "re");
    if (f == NULL) {
        *errnop = errno;
        return NSS_STATUS_UNAVAIL;
    }
    while ((tpl = fgetpwent(f)) != NULL)
        if (strcmp(tpl->pw_name, "blog") == 0)
            break;
    if (tpl == NULL) {
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
        copy_str(tpl->pw_shell, &result->pw_shell, &buf, &left)) {
        fclose(f);
        *errnop = ERANGE;
        return NSS_STATUS_TRYAGAIN;
    }
    result->pw_uid = tpl->pw_uid;
    result->pw_gid = tpl->pw_gid;
    fclose(f);
    return NSS_STATUS_SUCCESS;
}
