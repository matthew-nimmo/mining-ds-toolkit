#include <R.h>
#include <Rdefines.h>
#include "Rdmfile.h"

float inFloat(char* buf, char ByteSwapped);
void inString(char* buf, char* target, int size);
char inChar(char* buf);
void swapBytes(char* ptr);
SEXP Rdmread(SEXP fname);

void swapBytes(char* ptr)
{
   char tmp;

   tmp = ptr[0];
   ptr[0] = ptr[3];
   ptr[3] = tmp;

   tmp = ptr[1];
   ptr[1] = ptr[2];
   ptr[2] = tmp;
}

float inFloat(char* buf, char ByteSwapped)
{
   float f;

   memcpy(&f, buf, sizeof(float));
   if (ByteSwapped)
      swapBytes((char*) &f);

   return(f);
}

void inString(char* buf, char* target, int size)
{
   char f[size + 1];

   memcpy(&f, buf, size);
   f[size]=0x00;

   strcpy(target, f);
}

char inChar(char* buf)
{
   char f;

   memcpy(&f, buf, 1);

   return(f);
}

char *trim(char *s)
{
	char *ptr;
	if (!s)
		return NULL;   // handle NULL string
	if (!*s)
		return s;      // handle empty string
	for (ptr = s + strlen(s) - 1; (ptr >= s) && isspace(*ptr); --ptr);
		ptr[1] = '\0';
	return s;
}

SEXP Rdmread(SEXP fname)
{
	int pc=0, i, p, v, r, nv, nr, np, d, ne=0, ni=0, nrecs=0;
	SEXP df, varlabels, df_names, row_names;
	struct dm_field *vars;
	struct dm_header header;
	char tmp[5], *fn = NULL, ieee = 0;
	double fdate=0;

	fn = CHAR(STRING_ELT(fname, 0));
	FILE* fp = fopen(fn, "rb");
	PROTECT(fp); pc++;

	char buf[SIZE_OF_BUFFER];
	PROTECT(buf); pc++;
	if (fread(buf, sizeof(char), SIZE_OF_BUFFER, fp) != SIZE_OF_BUFFER)
		return(R_NilValue);

	// ---
	//fdate = inFloat(buf+96, ieee);
	//if ((fdate >= 720101) && (fdate <= 99991231))
	//	ieee = 1;

	inString(buf, header.fileName, 8);
	inString(buf+8, header.directory, 8);
	inString(buf+16, header.description, 64);
	inString(buf+80, header.owner, 8);
	header.ownerPerms = inFloat(buf+88, ieee);
	header.otherPerms = inFloat(buf+92, ieee);
	header.modifyDate = inFloat(buf+96, ieee);
	header.numFields = inFloat(buf+100, ieee);
	header.numPages = inFloat(buf+104, ieee);
	header.recsLastPage = inFloat(buf+108, ieee);

	// ---

	nv = (int) header.numFields;
	vars = (struct dm_field*) malloc(sizeof(struct dm_field) * nv);
	PROTECT(vars); pc++;
	for (v = 0; v < nv; v++)
	{
		inString(buf+112+(v*28), vars[v].name, 8);
		vars[v].type = inChar(buf+120+(v*28));
		vars[v].logicalRecPos = inFloat(buf+124+(v*28), ieee);
		vars[v].wordNumber = inFloat(buf+128+(v*28), ieee);
		vars[v].unit = inFloat(buf+132+(v*28), ieee);
		vars[v].size = 4;

		if(vars[v].type == 'N')
		{
			vars[v].ndefault = inFloat(buf+136+(v*28), ieee);
			ne++;
		}
		else
		{
			inString(buf+136+(v*28), tmp, 4);
			if(v == 0)
			{
				vars[v].cdefault = (char*) malloc(sizeof(char) * 5);
				strcpy(vars[v].cdefault, tmp);
				ne++;
			}
			else if(vars[v].wordNumber > 1)
			{
				i = v - ((int) vars[v].wordNumber - 1);
				strcat(vars[i].cdefault, tmp);
				vars[i].size = vars[i].size + 4;
				vars[v].size = 0;
			}
		}
		//If field is implicit
		if (vars[v].logicalRecPos > 0)
			ni++;
	}
printf("ne = %i\n", ne);
printf("ni = %i\n", ni);

	if (ni > 0) nrecs = (header.numPages - 2) * (508 / ni) + header.recsLastPage;
	np = header.numPages - 1;
	if (np < 0) np = 0;

	PROTECT(df = allocVector(VECSXP, nv)); pc++;
	PROTECT(varlabels = allocVector(STRSXP, nv)); pc++;
	for (v = 0; v < nv; v++)
	{
		if(vars[v].type == 'N')
			SET_VECTOR_ELT(df, v, allocVector(REALSXP, nrecs));
		else
			SET_VECTOR_ELT(df, v, allocVector(STRSXP, nrecs));
		SET_STRING_ELT(varlabels, v, mkChar(trim(vars[v].name)));
	}

	// ---

	d = 0;
	for (p = 0; p < np; p++)
	{
		// get this page and process it
		if (fread(buf, sizeof(char), SIZE_OF_BUFFER, fp) != SIZE_OF_BUFFER)
			return(R_NilValue);

		if (header.numFields > 0)
			nr = 508 / ni;
		else
			nr = 0;

		if (p == (np-1))
			nr = header.recsLastPage;

		// loop over records
		for (r = 0; r < nr; r++)
		{
			// loop over variables for this record
			for (v = 0; v < nv; v++)
			{
				i = (((r * ni) + (vars[v].logicalRecPos - 1)) * SIZE_OF_WORD);
				if (vars[v].type == 'N')
				{
					// check if variable is implicit or not
					if (vars[v].logicalRecPos > 0)
						REAL(VECTOR_ELT(df, v))[d] = (double) inFloat(buf + i, ieee);
					else
						REAL(VECTOR_ELT(df, v))[d] = vars[v].ndefault;

					if(REAL(VECTOR_ELT(df, v))[d] <= -(1e+30))
							REAL(VECTOR_ELT(df, v))[d] = NA_REAL;
				}
				else
				{
					if (vars[v].logicalRecPos > 0)
						SET_STRING_ELT(VECTOR_ELT(df, v), d, NA_STRING);
						//SET_STRING_ELT(VECTOR_ELT(df, v), d, CHARSXP("A"));
					else
						SET_STRING_ELT(VECTOR_ELT(df, v), d, mkChar(vars[v].cdefault));
				}
			}
			d++;
		}
	}

	// ---

	fclose(fp);

	setAttrib(df, R_NamesSymbol, varlabels);

    PROTECT(df_names = mkString("data.frame")); pc++;
    setAttrib(df, R_ClassSymbol, df_names);

	//setAttrib(df, R_RowNamesSymbol, row_names);
	PROTECT(row_names = allocVector(INTSXP, 2)); pc++;
	INTEGER(row_names)[0] = NA_INTEGER;
	INTEGER(row_names)[1] = nrecs;
	setAttrib(df, R_RowNamesSymbol, row_names);

	UNPROTECT(pc);

	return(df);
}
