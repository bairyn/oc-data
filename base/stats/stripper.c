/*
stripper.c

strips ip, guid and realname from stats
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <dirent.h>
#include <errno.h>

#define MAX_LINE_SIZE 4096
#define REPLACE_STRING "nodate\x01guid\x01ip\x01noname"
#define REPLACE_STRING_LEN (strlen((REPLACE_STRING)))

static char cwd[FILENAME_MAX];

static const char *find(const char *str, const char *find, const char *endStrExclusive)
#define dfind(arg0, arg1, arg2) ((char *)(find((arg0), (arg1), (arg2))))
{
	int breaking = 0;
	const char *findPos;
	const char *strPos;

	if(endStrExclusive)
	{
		while(str < endStrExclusive)
		{
			findPos = find;
			strPos = str;

			while(*findPos)
			{
				if(*strPos++ != *findPos++)
				{
					str++;
					breaking = 1;
					break;
				}
			}

			if(!breaking)
				return str;

			breaking = 0;
		}
	}
	else
	{
		while(*str)
		{
			findPos = find;
			strPos = str;

			while(*findPos)
			{
				if(*strPos++ != *findPos++)
				{
					str++;
					breaking = 1;
					break;
				}
			}

			if(!breaking)
				return str;

			breaking = 0;
		}
	}

	return NULL;
}

int readLine(char **n, char *b, char *endData)
{
	char *p = b;

	while(p < endData)
	{
		if(*p == '\n')
		{
			*n = p;
			return 1;
		}

		p++;
	}

	return 0;
}

static void strip(const char *name)
{
	int i;
	int len, lenOffset;
	char *data = NULL, *endData = NULL;
	char *line;
	char *n = NULL;  /* pointer to next newline character */
	FILE *fin = NULL;
	FILE *fout = NULL;
	char buf[FILENAME_MAX];

	strncpy(buf, cwd, FILENAME_MAX);
	strncat(buf, "/", FILENAME_MAX - strlen(buf));
	strncat(buf, name, FILENAME_MAX - strlen(buf));

	if(strcmp(name, "win.dat") && strcmp(name, "med.dat"))
		return;

	if(!(fin = fopen(buf, "r")))
	{
		fprintf(stderr, "strip() error: could not open '%s'\n", buf);
		exit(1);
	}

	fseek(fin, 0, SEEK_END);
	len = ftell(fin);
	fseek(fin, 0, SEEK_SET);

	if(len < 3)  // empty or invalid file
	{
		fclose(fin);
		return;
	}

	if(!(data = malloc(len << 1)))  // more in case replacing results in a larger file (rare case)
	{
		fclose(fin);
		fprintf(stderr, "strip() error: could not allocate %d bytes for '%s'\n", len, buf);
		exit(1);
	}
	memset(data, 0, len << 1);

	endData = data + (len << 1);

	if(fread(data, 1, len, fin) != len)
	{
		fclose(fin);
		free(data);
		fprintf(stderr, "strip() error: error reading '%s'\n", buf);
		exit(1);
	}

	// strip
	line = data;

	// skip first line
	for(i = 0; i < 1; i++)
	{
		if(!readLine(&n, line, endData))
		{
			break;
		}
		line = n + 1;
	}
	if(line)
	{
		while(line - data < len && readLine(&n, line, endData))
		{
			for(i = 0; i < 3; i++)  // skip the first three seperation bytes
			{
				if((line = dfind(line, "\x01", n) + 1) <= (char *)0x000000FF)
				{
					fclose(fin);
					free(data);
					fprintf(stderr, "Warning: '%s' is not properly formatted\n", buf);
					return;
				}
			}

			// now we are at the beginning of what needs to be replaced, so
			// replace
			lenOffset = REPLACE_STRING_LEN - (n -  line);
			if(data + len + lenOffset >= endData)
			{
				fclose(fin);
				free(data);
				fprintf(stderr, "Warning: the difference of sizes between the new file of '%s' will be too big to hold into memory\n", buf);
				return;
			}

			memmove(n + lenOffset, n, data + len - n);
			memcpy(line, REPLACE_STRING, REPLACE_STRING_LEN);

			len += lenOffset;
			// finished replacing line

			line = n + lenOffset + 1;
		}
	}

	fclose(fin);

	// now write the output
	fout = fopen(buf, "w");
	fwrite(data, 1, len, fout);
	fclose(fout);

	free(data);
}

static void traverse(void)
{
	DIR *d;
	struct dirent *dir;
	char buf[FILENAME_MAX];

	strncpy(buf, cwd, FILENAME_MAX);
	d = opendir(buf);

	if(!d)
	{
		fprintf(stderr, "could not open a directory: '%s'\n", buf);
		exit(errno);
	}

	while(dir = readdir(d))
	{
		if(dir->d_name[0] == '.')
			continue;
		if(dir->d_type == DT_REG)
		{
			strip(dir->d_name);
		}
		if(dir->d_type == DT_DIR)
		{
			char tmp[FILENAME_MAX];
			strncpy(tmp, buf, FILENAME_MAX);
			strncat(buf, "/", FILENAME_MAX - strlen(buf));
			strncat(buf, dir->d_name, FILENAME_MAX - strlen(buf));
			strncpy(cwd, buf, FILENAME_MAX);
			traverse();
			strncpy(buf, tmp, FILENAME_MAX);
			strncpy(cwd, buf, FILENAME_MAX);
		}
	}

	closedir(d);
}

int main(int argc, char **argv)
{
	int i;
	int force = 0;
	DIR *d;
	struct dirent *dir;
	char *pwd = getenv("PWD");

	for(i = 1; i < argc; i++)
	{
		if(!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help"))
		{
			fprintf(stdout, "Usage: %s [-h/--help] [-f/--force]\n", argv[0]);
			return 0;
		}
		if(!strcmp(argv[i], "-f") || !strcmp(argv[i], "--force"))
		{
			force = 1;
		}
	}

	if(!pwd || !find(pwd, "oc-data", NULL))
	{
		fprintf(stderr, "oc-data was not found in pwd.  Use -f to continue anyway\n");
		return 1;
	}

	strncpy(cwd, pwd, FILENAME_MAX);

	traverse();

	return 0;
}
