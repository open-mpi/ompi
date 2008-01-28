/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/zam/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2007                                                **
**  Forschungszentrum Juelich, Zentralinstitut fuer Angewandte Mathematik  **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#include <iostream>
  using std::cerr;
#include <vector>
  using std::vector;
#include <cctype>
  using std::tolower;
  using std::toupper;
#include <string>
  using std::getline;
#include <algorithm>
  using std::transform;
  using std::sort;
#include <functional>
  using std::greater;
#include <cstring>
  using std::strlen;

#ifdef EBUG
#  include <iomanip>
   using std::setw;
#endif

#include "opari.h"
#include "handler.h"

namespace {
  void look_for(const string& lowline, const char* word,
                vector<string::size_type>& positions) {
    string::size_type s = 0;
    while ( (s = lowline.find(word, s)) != string::npos ) {
      positions.push_back(s);
      s += strlen(word);
    }
  }

  bool is_comment_line(string& lowline, string& line) {
    if ( lowline[0] == '!' || lowline[0] == '*' || lowline[0] == 'c' ) {
      // fixed form comment

      if ( lowline[1] == '$' &&
           lowline.find_first_not_of(" \t0123456789", 2) > 5 ) {
        // OpenMP Conditional Compilation
        lowline[0] = ' ';
        lowline[1] = ' ';
        return false;
      } else if ( lowline[1] == 'p' && lowline[2] == '$' &&
           lowline.find_first_not_of(" \t0123456789", 3) > 5 ) {
        // POMP Conditional Compilation
        lowline[0] = line[0] = ' ';
        lowline[1] = line[1] = ' ';
        lowline[2] = line[2] = ' ';
        return false;
      } else {
        return true;
      }
    }

    string::size_type s = lowline.find_first_not_of(" \n");
    if ( s != string::npos && lowline[s] == '!' ) {
      // free form full line comment

      string::size_type c = lowline.find("!$ ");
      if ( c == s ) {
        // OpenMP Conditional Compilation
        lowline[s]   = ' ';
        lowline[s+1] = ' ';
        return false;
      }
      c = lowline.find("!p$ ");
      if ( c == s ) {
        // POMP Conditional Compilation
        lowline[s]   = line[s] = ' ';
        lowline[s+1] = line[s+1] = ' ';
        lowline[s+2] = line[s+2] = ' ';
        return false;
      }
      return true;
    }
    return false;
  }

  void del_strings_and_comments(string& lowline, char& inString) {
    // zero out string constants and free form comments
    for (unsigned i=0; i<lowline.size(); ++i) {
      if ( inString ) {
        // inside string
        if ( lowline[i] == inString ) {
          lowline[i] = '@';
          ++i;
          if ( i >= lowline.size() ) {
            // eol: no double string delimiter -> string ends
            inString = 0;
            break;
          }
          if ( lowline[i] != inString ) {
            // no double string delimiter -> string ends
            inString = 0;
            continue;
          }
        }
        lowline[i] = '@';
      }

      else if ( lowline[i] == '!' ) {
        /* -- zero out partial line F90 comments -- */
        for (; i<lowline.size(); ++i) lowline[i] = 'C';
        break;
      }

      else if ( lowline[i] == '\'' || lowline[i] == '\"' ) {
        inString = lowline[i];
        lowline[i] = '@';
      }
    }
  }

  void replace_openmp_api_calls(string& lowline, string& line) {
    // replace call to omp_*_lock routines
    vector<string::size_type> positions;
    look_for(lowline, "omp_init_lock", positions);
    look_for(lowline, "omp_destroy_lock", positions);
    look_for(lowline, "omp_set_lock", positions);
    look_for(lowline, "omp_unset_lock", positions);
    look_for(lowline, "omp_test_lock", positions);
    look_for(lowline, "omp_init_nest_lock", positions);        /*2.0*/
    look_for(lowline, "omp_destroy_nest_lock", positions);     /*2.0*/
    look_for(lowline, "omp_set_nest_lock", positions);         /*2.0*/
    look_for(lowline, "omp_unset_nest_lock", positions);       /*2.0*/
    look_for(lowline, "omp_test_nest_lock", positions);        /*2.0*/
    sort(positions.begin(), positions.end(), greater<string::size_type>());
    for (unsigned i=0; i<positions.size(); ++i) {
      line.replace(positions[i], 3, "POMP");
      line[positions[i]+5] = toupper(line[positions[i]+5]);
    }
  }

  struct fo_tolower : public std::unary_function<int,int> {
    int operator()(int x) const {
      return std::tolower(x);
    }
  };
}

void process_fortran(istream& is, const char* infile, ostream& os,
                     bool addSharedDecl) {
  string line;
  int lineno = 0;
  OMPragma* currPragma = 0;
  bool needPragma = false;
  char inString = 0;
  string::size_type pstart = string::npos;
  string::size_type lstart = string::npos;

  while ( getline(is, line) ) {
    /* workaround for bogus getline implementations */
    if ( line.size() == 1 && line[0] == '\0' ) break;

    ++lineno;
    string lowline(line);
    transform(line.begin(), line.end(), lowline.begin(), fo_tolower());

    if ( inString ) {
      if ( ! is_comment_line(lowline, line) ) {
        del_strings_and_comments(lowline, inString);
        if (  instrument_locks() ) replace_openmp_api_calls(lowline, line);
      }
      os << line << '\n';
#     ifdef EBUG
      cerr << setw(3) << lineno << ":S+: " << line << '\n';
#     endif

    } else if ( line.size() &&
      (lowline[0] == '!' || lowline[0] == 'c' || lowline[0] == '*') && (
         (lowline[1] == '$' &&
                 ( 
                   (lowline[2] == 'p' && lowline[3] == 'o' &&
                    lowline[4] == 'm' && lowline[5] == 'p')
                 ||
                   (lowline[2] == 'o' &&
                    lowline[3] == 'm' && lowline[4] == 'p')) )
	 ||
	 (lowline[1] == 'p' && lowline[2] == 'o' &&
	  lowline[3] == 'm' && lowline[4] == 'p' && lowline[5] == '$')
      )) {

      int pomp = ((lowline[1] == 'p') || (lowline[2] == 'p'));

      /*
       * fix form omp directive
       */
      if ( lowline[5+pomp]==' ' || lowline[5+pomp]=='\t'
                                || lowline[5+pomp]=='0' ) {
        // new directive
        if ( currPragma ) {
          // if necessary process last complete directive
          process_pragma(currPragma, os);
          currPragma = 0;
        }
        currPragma = new OMPragmaF(infile, lineno, 6+pomp, lowline, pomp,
                                   addSharedDecl);

      } else {
        // continuation directive line
        if ( currPragma ) {
          currPragma->lines.push_back(lowline);
        } else {
          cerr << infile << ":" << lineno
               << ": ERROR: invalid continuation line\n";
          cleanup_and_exit();
        }
      }

    } else if ( line.size() &&
                (lstart = lowline.find_first_not_of(" \t")) != string::npos &&
                ((lstart == (pstart = lowline.find("!$omp"))) ||
                 (lstart == (pstart = lowline.find("!$pomp"))) ||
                 (lstart == (pstart = lowline.find("!pomp$"))))
              ) {

      int pomp = ((lowline[pstart+1] == 'p') || (lowline[pstart+2] == 'p'));

      /*
       * free form omp directive
       */
      if ( needPragma ) {
        // continuation directive line
        currPragma->lines.push_back(lowline);
      } else {
        // new directive
        currPragma
                = new OMPragmaF(infile, lineno, pstart+5+pomp, lowline, pomp,
                                addSharedDecl);
      }
      string::size_type com = lowline.find("!", pstart+4+pomp);
      if ( com != string::npos ) --com;
      string::size_type amp = lowline.find_last_not_of(" \t", com);
      if ( lowline[amp] == '&' ) {
        // last non-white non-comment character == '&' --> continuation
        needPragma = true;
      } else {
        // complete
        needPragma = false;
        process_pragma(currPragma, os);
        currPragma = 0;
      }

    } else {
      /*
       * normal line
       */
      if ( needPragma ) {
        cerr << infile << ":" << lineno-1
             << ": ERROR: missing continuation line\n";
        cleanup_and_exit();
      } else if ( currPragma ) {
        // if necessary process last complete directive
        process_pragma(currPragma, os);
        currPragma = 0;
      }

      if ( is_comment_line(lowline, line) ) {
        os << line << '\n';
#       ifdef EBUG
        cerr << setw(3) << lineno << ":C : " << line << '\n';
#       endif
      } else {
        del_strings_and_comments(lowline, inString);
        if ( instrument_locks() ) {
          replace_openmp_api_calls(lowline, line);
        }
        os << line << '\n';
        extra_handler(lineno, os);
#       ifdef EBUG
        cerr << setw(3) << lineno << ":  : " << line << '\n';
#       endif
      }
    }
  }
}
