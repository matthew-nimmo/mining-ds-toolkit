#define SIZE_OF_BUFFER  2048
#define SIZE_OF_WORD 4

struct dm_header
{
   char fileName[9];
   char directory[9];
   char description[65];
   char owner[9];
   float ownerPerms;
   float otherPerms;
   float modifyDate;
   float numFields;
   float numPages;
   float recsLastPage;
};

struct dm_field
{
  char name[9];
  char type;
  float logicalRecPos;
  float wordNumber;
  float unit;
  float ndefault;
  char *cdefault;
  int size;
};
