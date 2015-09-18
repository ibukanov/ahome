#ifndef _STRTONUM_H
#define _STRTONUM_H

struct StrNum {
  enum State {OK, BAD_STRING, OUT_OF_RANGE};
  
  StrNum() : status(OK) { }
  State getStatus() { return status; }
  bool ok() { return status == OK; }
  
  long tol(const char *str);
  unsigned long toul(const char *str);
  int toi(const char *str);
  unsigned int toui(const char *str);
  double tof(const char *str);
  bool tob(const char *str);

  private: 
    State status;
    void setStatus(State _status) { status = _status; }
};


struct ResourceSource {
  virtual const char* accessStr(const char *name, const char *argList) = 0;
  virtual bool onParseError(
    const char* name, const char* badString, const char* typeName) = 0;
  virtual bool onOutOfRangeNumber(
    const char* name, const char* numberBegin, const char* numberEnd, 
    const char* typeName) = 0;
};  

int getInt(
  ResourceSource&, const char *name, const char *argList = NULL);

unsigned int getUnsignedInt(
  ResourceSource&, const char *name, const char *argList = NULL);

long getLong(
  ResourceSource&, const char *name, const char *argList = NULL);

unsigned long getUnsignedLong(
  ResourceSource&, const char *name, const char *argList = NULL);

double getDouble(
  ResourceSource&, const char *name, const char *argList = NULL);

bool getBool(
  ResourceSource&, const char *name, const char *argList = NULL);
             
/* String is allocated by new char[] and should be freed by delete[]
 */ 
char* getString(
  ResourceSource&, const char *name, const char *argList = NULL);

unsigned int getStringToBuffer(
  char *buffer, size_t size, 
  ResourceSource&, const char *name, const char *argList = NULL);

struct ClosedInterval {
  double begin;
  double end;

  unsigned int pointCnt;
  double step;

  ClosedInterval(): begin(0.0), end(0.0), pointCnt(1), step(0.0) { }

  unsigned pointCount() const { return pointCnt; }

  double point(unsigned int i) const { return begin + i * step; }  
};
   
ClosedInterval getInterval(
  ResourceSource&, const char *name, const char *argList = NULL);


#endif /* _STRTONUM_H */
