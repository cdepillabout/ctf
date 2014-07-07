
# LEENODE

## Description

http://203.66.57.98/

### Hint

(none)

## Solution

### WriteUps

- https://github.com/ctfs/write-ups/tree/master/hitcon-ctf-2014/leenode

### My Notes

1. When failing a login, you are presented a page that says `Apache/2.0.65
   (Unix) JRun/4.0 Server`.

1. When looking for JRun vulnerabilities, there are things like
   [CVE-2004-0928](http://www.cvedetails.com/cve/CVE-2004-0928/).

1. The vulnerability says that you should be able to access a page like
   http://203.66.57.98/a;.jsp in order to see the jsp source, but you can tell
   this error page is handled by apache.

1. However, you can use a double escaping trick. http://203.66.57.98/a%253b.jsp

1. Apache blocks attempts to read .htaccess.

1. JRun apparently accepts '\' as a slash operator, so we can use a url like
   the following to read .htaccess and .htpasswd:

   http://203.66.57.98/.%5Cadmin%5C.htaccess%253b.jsp

   Apparently Apache hands this off to JRun because it ends with .jsp, and JRun
   is fooled by the '\'. This lets you read the .htaccess and .htpasswd files.

1. The .htpasswd file can be cracked with John.

### Takeaways

- If a funny/old web/application server is being used, check for
  vulnerabilities.

- 500 errors are often returned by Apache.

- Check for double encoding errors. (For instance, ';' actually double encoded
  as '%253b' instead of '%3b').

- I guess there are some web/application servers that will accept a backslash
  as a path name.  Check for this as well: %5C

- Check for .htaccess files.  Getting a .htpasswd file basically means you can
  crack it with John.
