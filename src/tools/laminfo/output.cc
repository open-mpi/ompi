//
// $HEADER$
//
/** @file **/

#include "lam_config.h"

#include <iostream>
#include <string>

#include "tools/laminfo/laminfo.h"

using namespace std;
using namespace laminfo;

#define LAM_max(a,b) (((a) > (b)) ? (a) : (b)) 


//
// Private variables
//

static int centerpoint = 24;
static int screen_width = 78;


// 
// Prints the passed strings in a pretty or parsable format.
//
void laminfo::out(const string& pretty_message, const string &plain_message,
                  const string& value)
{
  if (pretty) {
    string::size_type pos, max_value_width;
    string spaces(LAM_max(centerpoint - pretty_message.length(), 0), ' ');
    string v = value;
    string filler;

    max_value_width = screen_width - spaces.length() -
      pretty_message.length() - 2;
    filler = spaces + pretty_message + ": ";

    while (true) {
      if (v.length() < max_value_width) {
        cout << filler << v << endl;
        break;
      } else {
        string spaces(centerpoint + 2, ' ');

        // Work backwards to find the first space before
        // max_value_width

        pos = v.rfind(' ', max_value_width);
        if (string::npos == pos) {

          // No space found < max_value_width.  Look for the first
          // space after max_value_width.

          pos = v.find(' ', max_value_width);

        if (string::npos == pos) {

            // There's just no spaces.  So just print it and be done.

            cout << filler << v << endl;
            break;
          } else {
            cout << filler << v.substr(0, pos) << endl;
            v = v.substr(pos + 1);
          }
        } else {
          cout << filler << v.substr(0, pos) << endl;
          v = v.substr(pos + 1);
        }

        // Reset for the next iteration

        filler = spaces;
      }
    }
  } else {
    cout << plain_message << ":" << value << endl;
  }
}


// 
// Prints the passed integer in a pretty or parsable format.
//
void laminfo::out(const string& pretty_message, const string &plain_message, 
                  int value)
{
  if (laminfo::pretty) {
    string spaces(LAM_max(centerpoint - pretty_message.length(), 0), ' ');
    cout << spaces << pretty_message << ": " << value << endl;
  } else {
    cout << plain_message << ":" << value << endl;
  }
}
