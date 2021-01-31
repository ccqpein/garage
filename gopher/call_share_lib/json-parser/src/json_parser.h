struct Method {
  char* Name;
  char* Doc;
  char* Types;
  char* Parameters;
  char* Requireds;
  char* Descriptions;
};

struct Method* json_parser(char*);

void hello();
