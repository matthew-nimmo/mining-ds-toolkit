#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "dmfile.h"

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

inline float inFloat(char* buf, char ByteSwapped)
{
   float f;

   memcpy(&f, buf, sizeof(float));
   if (ByteSwapped)
      swapBytes((char*) &f);

   return(f);
}

inline inString(char* buf, char* target, int size)
{
   char f[size + 1];

   memcpy(&f, buf, size);
   f[size]=0x00;

   strcpy(target, f);
}

inline char inChar(char* buf)
{
   char f;

   memcpy(&f, buf, 1);

   return(f);
}

void do_readDM(char* fname)
{
   //SEXP fname, result;
   FILE* fp;

/*
   if ((sizeof(double) != 8) | (sizeof(int) != 4) | (sizeof(float) != 4))
      error(_("can not yet read Datamine .dm on this platform"));

   if (!isValidString(fname = CADR(call)))
      error(_("first argument must be a file name\n"));

   fp = fopen(R_ExpandFileName(CHAR(STRING_ELT(fname,0))), "rb");
   if (!fp)
      error(_("unable to open file"));

   result = R_LoadDMData(fp);
*/
   fp = fopen(fname, "rb");

   R_LoadDMData(fp);

   fclose(fp);

   //return result;
}

void R_LoadDMData(FILE* fp)
{
   int d, i, j, nd = 0, np, nr, nv, ni = 0, ne = 0, p, r, v;
   char buf[SIZE_OF_BUFFER], tmp[5];

   struct dm_header header;
   struct dm_field *Vars, *tmpVars;

   // get header page
   if (fread(buf, sizeof(char), SIZE_OF_BUFFER, fp) != SIZE_OF_BUFFER)
      //error(_("a binary read error occurred"));
      printf("error");

   // get header
   inString(buf, header.fileName, 8);
   inString(buf+8, header.directory, 8);
   inString(buf+16, header.description, 64);
   inString(buf+80, header.owner, 8);

   header.ownerPerms = inFloat(buf+88, 0);
   header.otherPerms = inFloat(buf+92, 0);
   header.modifyDate = inFloat(buf+96, 0);
   header.numFields = inFloat(buf+100, 0);
   header.numPages = inFloat(buf+104, 0);
   header.recsLastPage = inFloat(buf+108, 0);

   //printHeader(header);

   // get fields
   nv = (int) header.numFields;
   tmpVars = (struct dm_field*) malloc(sizeof(struct dm_field) * nv);
   for (v = 0; v < nv; v++)
   {
      inString(buf+112+(v*28), tmpVars[v].name, 8);
      tmpVars[v].type = inChar(buf+120+(v*28));
      tmpVars[v].logicalRecPos = inFloat(buf+124+(v*28), 0);
      tmpVars[v].wordNumber = inFloat(buf+128+(v*28), 0);
      tmpVars[v].unit = inFloat(buf+132+(v*28), 0);
      tmpVars[v].size = 4;

      if(tmpVars[v].type == 'N')
      {
        tmpVars[v].ndefault = inFloat(buf+136+(v*28), 0);
        ne++;
      }
      else
      {
        inString(buf+136+(v*28), tmp, 4);
        if(v == 0)
        {
           tmpVars[v].cdefault = (char*) malloc(sizeof(char) * 5);
           strcpy(tmpVars[v].cdefault, tmp);
           ne++;
        }
        else if(tmpVars[v].wordNumber > 1)
        {
          i = v - ((int) tmpVars[v].wordNumber - 1);
          strcat(tmpVars[i].cdefault, tmp);
          tmpVars[i].size = tmpVars[i].size + 4;
          tmpVars[v].size = 0;
        }
      }

      //If field is implicit
      if (tmpVars[v].logicalRecPos > 0)
        ni++;
   }

   if (ni > 0)
      nd = (header.numPages - 2) * (508 / ni) + header.recsLastPage;

   i = 0;
   Vars = (struct dm_field*) malloc(sizeof(struct dm_field) * ne);
   for (v = 0; v < nv; v++)
   {
      if(tmpVars[v].size > 0)
      {
         Vars[i] = tmpVars[v];
         if(Vars[i].type == 'N')
           Vars[i].ndata = (float*) malloc(sizeof(float) * nd);
         else
            Vars[i].cdata = (char**) malloc(sizeof(char) * nd);

         //printField(Vars[i]);
         i++;
      }
   }
   free(tmpVars);

   // get data
   np = header.numPages - 1;
   if (np < 0)
      np = 0;

   d = 0;
   for (p = 0; p < np; p++)
   {
      // get this page and process it
      if (fread(buf, sizeof(char), SIZE_OF_BUFFER, fp) != SIZE_OF_BUFFER)
      {
         ne = 0;
         if (Vars) free(Vars);
         return;
      }

      if (header.numFields > 0)
         nr = 508 / ni;
      else
         nr = 0;

      if (p == (np-1))
         nr = header.recsLastPage;

      // loop over records
      nv = ne;
      for (r = 0; r < nr; r++)
      {
         // loop over variables for this record
         for (v = 0; v < nv; v++)
         {
            i = (((r * ni) + (Vars[v].logicalRecPos - 1)) * SIZE_OF_WORD)
            if (Vars[v].type == 'N')
            {
               // check if variable is implicit or not
               if (Vars[v].logicalRecPos > 0)
                  Vars[v].ndata[d] = inFloat(buf + i, 1 * sizeof(float));
               else
                  Vars[v].ndata[d] = Vars[v].ndefault;
            }
            else
            {
               Vars[v].cData[d] = (char*) malloc(sizeof(char) * Vars[v].size);
               if (Vars[v].logicalRecPos > 0)
               {
                  inString(buf + i, Vars[v].size);
                  strcpy(Vars[v].cData[d], tmpstr);
               }
               else
               {
                  strcpy(Vars[v].cData[d], Vars[v].cdefault);
               }
            }
         }
         d++;
      }
   }
   free(Vars);
}

void printHeader(struct dm_header h)
{
  printf("File Name: %s\n", h.fileName);
  printf("Directory: %s\n", h.directory);
  printf("Description: %s\n", h.description);
  printf("Owner: %s\n", h.owner);

  printf("Owner Permisions: %i\n", (int) h.ownerPerms);
  printf("Other Permisions: %i\n", (int) h.otherPerms);
  printf("Modify Date: %i\n", (int) h.modifyDate);
  printf("Number of Fields: %i\n", (int) h.numFields);
  printf("Number of Pages: %i\n", (int) h.numPages);
  printf("Records in Last Page: %i\n", (int) h.recsLastPage);
}

void printField(struct dm_field f)
{
  printf("Name: %s\n", f.name);
  printf("Type: %c\n", f.type);

  printf("Logical Position: %i\n", (int) f.logicalRecPos);
  printf("Word Number: %i\n", (int) f.wordNumber);
  printf("Unit: %i\n", (int) f.unit);
  printf("Size: %i\n", (int) f.size);

  if(f.type == 'N')
     printf("Default Value: %f\n", (int) f.ndefault);
  else
     printf("Default Value: %f\n", (int) f.cdefault);
}

int main()
{
   do_readDM("_collars.dm");

   return(0);
}
