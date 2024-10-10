#include <sys/ioctl.h>
#include <stdio.h>
#include <unistd.h>

int terminal_width ()
{
    struct winsize w;
    ioctl(STDOUT_FILENO, TIOCGWINSZ, &w);

    return w.ws_col;
}
