/* bbt_terminal.c -- Thin C wrapper for ioctl(TIOCSWINSZ)
 *
 * bbt gives the pseudo terminal allocated to the interactive commands
 * a fixed window size, so that a program querying its terminal size
 * behaves as on a real terminal (cf. docs/proposed_features/pty.md).
 *
 * ioctl is variadic (int ioctl(int, unsigned long, ...)), so Ada
 * cannot import it directly: this wrapper gives it a fixed signature,
 * and lets the C preprocessor resolve the TIOCSWINSZ constant, that
 * differs between Linux (0x5414) and macOS (0x80087468).
 */

#include <sys/ioctl.h>

int bbt_set_winsize(int fd, int rows, int cols) {
    struct winsize ws;
    ws.ws_row = (unsigned short)rows;
    ws.ws_col = (unsigned short)cols;
    ws.ws_xpixel = 0;
    ws.ws_ypixel = 0;
    return ioctl(fd, TIOCSWINSZ, &ws);
}
