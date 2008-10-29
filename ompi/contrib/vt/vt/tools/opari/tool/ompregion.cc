/****************************************************************************
**  SCALASCA    http://www.scalasca.org/                                   **
**  KOJAK       http://www.fz-juelich.de/jsc/kojak/                        **
*****************************************************************************
**  Copyright (c) 1998-2008                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYING in the package base directory for details       **
****************************************************************************/

#include "ompregion.h"
#include <iomanip>
  using std::setw;
  using std::setfill;

OMPRegion::OMPRegion(const string& n, const string& file,
                     int i, int bfl, int bll, bool outr)
  : name(n), file_name(file), id(i),
    begin_first_line(bfl), begin_last_line(bll),
    end_first_line(0), end_last_line(0),
    num_sections(0), noWaitAdded(false), outer_reg(outr), enclosing_reg(0) {
  enclosing_reg = outer_ptr;
  if (outer_reg) outer_ptr = this;
  if (outer_ptr) outer_ptr->descrs.insert(id);
}

void OMPRegion::generate_header(ostream& os) {
  os << "#include \"pomp_lib.h\"\n\n";
}

void OMPRegion::generate_descr(ostream& os) {
  os << "struct ompregdescr omp_rd_" << id << " = {\n";
  os << "  \"" << name << "\", \"" << sub_name << "\", "
     << num_sections << ", \"" << file_name << "\", "
     << begin_first_line << ", " << begin_last_line << ", "
     << end_first_line << ", " << end_last_line << "\n};\n\n";

  if (descrs.size()) {
    os << "#define POMP_DLIST_" << setw(5) << setfill('0') << id
       << " shared(";
    for (set<int>::const_iterator it = descrs.begin();
         it != descrs.end(); ++it) {
      if (it != descrs.begin()) os << ",";
      os << "omp_rd_" << *it;
    }
    os << ")\n\n";
  }
}

void OMPRegion::finish() {
  if (outer_reg) outer_ptr = enclosing_reg;
}

OMPRegion* OMPRegion::outer_ptr = 0;
