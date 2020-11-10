#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// function takes param1 file containing substitution data
// and does substitution on all lines
// param2 is the input file to be translated
// param3 is the output file
// the param1 file has the following format
//    line 1 -- a character string to be put at the start of every line
//    line 2 etc substitution data (see below)
//    line n -- **** marks the end of substitution data
//    line n+1 etc is then copied to the output

#define BUFFSZ 100000
#define NKEYS 1000

char *big_buff;
int verbose = 1;
char sp50[] = "                                                  ";

void main(int argc, char **argv)
{
    FILE *fin = stdin;
    FILE *fsubs = NULL;
    FILE *fout = stdout;
    char *in_line, *ws, *k, **key, **value;
    int *len; // length values for each key
    int key_count = 0;
    int subs_count = 0;
    int linecount = 0;
    int i, j;
    int tabpos = 0;
    char sepchar = ':'; // separator character between key and substitution string
    int kc[256];        // key count position for each initial character
    int ks[256];        // key count position for each initial character

    char *linestart;

    big_buff = (char *)malloc(BUFFSZ);

    for (i = 0; i < 256; i++) // initialise counts to zero
    {
        kc[i] = 0;
        ks[i] = 0;
    }

    if (argc < 3)
    {
        fprintf(stderr, "Usage: %s subs_file input_file [output_file]\n", *argv);
        exit(1);
    }

    fsubs = fopen(argv[1], "r");
    if (fsubs == NULL)
    {
        perror(argv[1]);
        exit(1);
    }

    key = (char **)malloc(NKEYS * sizeof(char *));
    value = (char **)malloc(NKEYS * sizeof(char *));

    *big_buff = 'z'; // anything not white space

    linestart = strdup(fgets(big_buff + 1, BUFFSZ, fsubs));
    k = linestart + strlen(linestart);
    while (k >= linestart && *--k < ' ')
        ;
    *++k = 0; // chop of trailing CRLF -- null line just plants spurious zero

    while (fgets((in_line = big_buff + 1), BUFFSZ, fsubs) != NULL && memcmp(in_line, "****", 4) != 0)
    {
        char *ii = in_line;
        while (*++ii != 0 && *ii != sepchar)
            ;
        if (*ii != 0) // if we found colon before end of string
        {
            *ii = 0;              // terminate string that is the key
            if (*(ii - 1) == ' ') // ignore a space before colon
                *(ii - 1) = 0;
            if (*++ii == ' ') // ignore a space after colon
                ii++;
            k = ii + strlen(ii);
            while (*--k <= ' ')
                ;
            *++k = 0; // strip trailing space
            key[key_count] = strdup(in_line);
            value[key_count++] = strdup(ii);
        }
        else if (*in_line == sepchar) // redefine sepchar
            sepchar = in_line[1];
        else if (*in_line == '\t') // tab position specifier
        {
            tabpos = atoi(in_line + 1); // just remember where first tab should be
            printf("Tabs at %d\n", tabpos);
        }
    }

    j = key_count;         // sort the keys ...
    while ((i = --j) >= 1) // ... in order of length to ensure ...
    {
        char *kj, *ki;
        while (--i >= 0)                                    // ... longest match first
            if (*(kj = key[j]) < *(ki = key[i])             // sort first on initial letter ...
                || (*kj == *ki && strlen(kj) < strlen(ki))) // ... then on length
            {
                ws = key[j];
                key[j] = key[i];
                key[i] = ws;
                ws = value[j];
                value[j] = value[i];
                value[i] = ws;
            }
    }
    printf("Sorted %d keys\n", key_count);
    len = (int *)malloc(key_count * sizeof(int));

    for (i = 0; i < key_count; i++)
    {
        kc[*(key[i]) & 255] = i + 1; // record the highest count for each intial character
        len[i] = strlen(key[i]);     // computer the lengths only once
    }
    for (i = key_count - 1; i >= 0; i--)
        ks[*(key[i]) & 255] = i; // record the lowest count for each intial character

    if (argc >= 4 && (fout = fopen(argv[3], "w")) == NULL)
    {
        perror(argv[3]);
        fclose(fsubs);
    }

    else if ((fin = fopen(argv[2], "r")) == NULL)
    {
        perror(argv[2]);
        fclose(fsubs);
        fclose(fout);
    }

    else // fin and fout opene OK - no previous file found
    {
        if (in_line != NULL) // if header text supplied
        {
            while (fgets((in_line = big_buff + 1), BUFFSZ, fsubs) != NULL)
                fprintf(fout, "%s", in_line);
        }
        fclose(fsubs);

        printf("Files opened OK\n");

        while (fgets((in_line = big_buff + 1), BUFFSZ, fin) != NULL)
        {
            char *out_line = in_line + strlen(in_line);
            char *pin, *pout, *k; // used to point at input and end of output
            int line_len;
            while ((*--out_line & 255) <= ' ')
                ;
            // chop trailing while space
            *++out_line = 0; // end the string
                             // *++out_line = 0;                // terminate null string
            sprintf(++out_line, linestart, linecount);
            // put in the start of line string
            line_len = strlen(in_line);

            if (*in_line == 0) // blank line
                fprintf(fout, "\n");
            else
            {
                linecount++; // only count non-blank lines as in Walgol
                k = in_line;
                pin = k - 1;
                while (*++pin != 0) // scan along the line
                {
                    if (*pin == '\t' && (j = tabpos - (pin - in_line)) > 0)
                    {
                        *pin = 0;                        // terminates k string
                        strcat(out_line, k);             // copy up to the tab
                        strcat(out_line, sp50 + 50 - j); // ... and substitute spaces to implement the tab
                        k = pin + 1;                     // move on past the tab character
                    }
                    else
                    {
                        j = kc[*pin & 255];
                        i = 0; // ks[*pin&255];
                        while (--j >= i && memcmp(pin, key[j], len[j]) != 0)
                            ;
                        if (j >= 0) // if we found a match
                        {
                            *pin = 0;                   // terminates k string
                            strcat(out_line, k);        // copy up to the start of recognition
                            strcat(out_line, value[j]); // ... and substitute the new string
                            k = pin + strlen(key[j]);   // move on past the recognised key
                            pin = k - 1;                // skip back for new scan
                            subs_count++;               // keep a tally of number of changes
                        }
                    }
                }
                strcat(out_line, k); // copy rest of the line
                fprintf(fout, "%s\n", out_line);
            }
        }

        if (fin != stdin)
            fclose(fin);
        if (fout != stdout)
            fclose(fout);

        printf("%d substitutions made\n", subs_count);
    }

    exit(0);
}
