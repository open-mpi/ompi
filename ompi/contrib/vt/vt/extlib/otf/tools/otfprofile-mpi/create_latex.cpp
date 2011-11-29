/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Robert Dietrich, Matthias Jurenz
*/

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <set>
#include <vector>
#include <limits>
#include <cmath>

#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>

using namespace std;

#include "create_latex.h"
#include "otfprofile-mpi.h"

#include "OTF_inttypes.h"
#include "OTF_Definitions.h"

#define FUNC_TABLE_LEN 50
#define CTR_TABLE_LEN 50

/* global constants to customize chart plots */
static const string COLOR_SEND = "red";
static const string COLOR_RECV = "blue";
static const string COLOR_YBAR = "green";
static const string COLOR_MINMAX= "black";
static const size_t FUNC_NAME_MAX_LEN = 32;
static const size_t CTR_NAME_MAX_LEN = 20;
static const int YBAR_SIZE = 4;
static const double PLOT_WIDTH = 16; //cm
static const double PLOT_HEIGHT = 8; //cm

/* will be set before latex creation and can be used in all local functions */
static bool clustered = false;
static vector<string> xLabels;
static int xLabelNum = 0;

/* The basic metric types for y bar charts */
enum metricType {
  INVOCATIONS,
  MSGLENGTH,
  DURATION
};
typedef enum metricType metric_t;

/* Pair of minimum and maximum value */
template <class type> class MinMaxPair {
  public:
    type min;
    type max;

    MinMaxPair( type a = (type) OTF_UINT64_MAX, type b= (type) 0) :
            min( a ), max( b ) {}
    ~MinMaxPair() {}
};

/* The minimum and maximum message data (count, byte, duration) */
struct MinMaxMsgData {
  //MinMaxPair<uint64_t> count;
  //MinMaxPair<uint64_t> bytes;
  MinMaxPair<double> count;
  MinMaxPair<double> bytes;
  MinMaxPair<double> duration;

  MinMaxMsgData( ) {
    initialize();
  }
  ~MinMaxMsgData( ) {}

  void initialize(){
    count.min = OTF_UINT64_MAX;
    count.max = 0;
    bytes.min = OTF_UINT64_MAX;
    bytes.max = 0;
    duration.min = numeric_limits<double>::max();
    duration.max = 0;
  }
};

/* function declarations */
static string convertBase2Exponent(uint64_t exp);
static void write_header(fstream& tex);
static void write_footer(fstream& tex);
static void write_p2pMsgRateMatrix(fstream& tex, struct AllData& alldata);

/*
 * Converts an exponent for basis 2 numbers into a string with shortcuts like
 * K for kilo, G for Giga, etc.
 *
 * @param exp the exponent
 *
 * @return the result string
 */
static string convertBase2Exponent(uint64_t exp)
{
  stringstream result;
  if(exp < 10){
    result << (1 << exp);
  }else{
    if(exp < 20){ // K
      result << (1 << (exp - 10)) << "K";
    }else{
      if(exp < 30){ // M
        result << (1 << (exp - 20)) << "M";
      }else{
        if(exp < 40){ // G
          result << (1 << (exp - 30)) << "G";
        }else{
          // T
          result << (1 << (exp - 40)) << "T";
        }
      }
    }
  }

  return result.str();
}

/*
 * K for kilo, G for Giga, for logarithmic dualis values
 *
 * @param min the minimum value
 * @param max the maximum value
 * @param unit the unit to retrieve
 *
 * @return the resulting divisor
 */
static uint64_t getScaleQuantifierLog2(double min, double max, char& unit)
{
  unit = ' ';

  /* after half of the steps, the limit like K, M, G should be reached */
  double limit = (min+max)/2; //steps/2*interval;

  if(limit > (1 << 30)){
    unit = 'G';
    return (1 << 30);
  }

  if(limit > (1 << 20)){
    unit = 'M';
    return (1 << 20);
  }

  if(limit > 1024){
    unit = 'K';
    return 1024;
  }

  return 1;
}

/*
 * K for kilo, M for Mega, G for Giga for logarithmic value base 10.
 *
 * @param min the minimum value
 * @param max the maximum value
 * @param unit the unit to retrieve
 * @param unitString reference to the string which shall be obtained
 *
 * @return the resulting divisor
 */
static uint64_t getScaleQuantifierLog10(double min, double max, char& unit,
                                        string& unitString)
{
  unit = ' ';

  /* after half of the steps, the limit like K, M, G should be reached */
  double limit = (min+max)/2; //steps/2*interval;

  if(limit > 1e12){
    unit = 'T';
    unitString = "Tera";
    return (uint64_t)1e12;
  }

  if(limit > 1e9){
    unit = 'G';
    unitString = "Giga";
    return (uint64_t)1e9;
  }

  if(limit > 1e6){
    unit = 'M';
    unitString = "Mega";
    return (uint64_t)1e6;
  }

  if(limit > 1e3){
    unit = 'K';
    unitString = "Kilo";
    return (uint64_t)1e3;
  }

  return 1;
}

/*
 * Try to create nice scale ticks.
 *
 * @param min reference to the minimum value of the scale
 * @param max reference to the maximum value of the scale
 * @param steps the number of tick values to be created
 */
static void makeNiceScaleTicks(double& min, double& max, uint8_t& steps)
{
  double interval = (max - min) / steps;

  /* get a nice maximum value */
  if(max > (uint64_t)max){
    max = (uint64_t)max + 1;
  }

  if(min - interval < 0) min = 0;
  else{
    min = (uint64_t)min;
  }

  //cout << "New min value: " << min << " New max value: " << max << endl;
}

/*
 * Converts the OTF collective definition to a string.
 *
 * @param id the OTF collective definition (input)
 * @param string the collective definition as string (output)
 */
static void collectiveId2String(uint64_t id, string& name)
{
  switch(id){
    case OTF_COLLECTIVE_TYPE_BARRIER: name = "BARRIER"; break;
    case OTF_COLLECTIVE_TYPE_ONE2ALL: name = "ONETOALL"; break;
    case OTF_COLLECTIVE_TYPE_ALL2ONE: name = "ALLTOONE"; break;
    case OTF_COLLECTIVE_TYPE_ALL2ALL: name = "ALLTOALL"; break;
    case OTF_COLLECTIVE_TYPE_UNKNOWN: name = "UNKNOWN"; break;
    default: name = "";
  }
}

/*
 * Write the document header including \begin{document} and general
 * 
 * @param tex the latex file output stream
 */
static void write_header(fstream& tex)
{
	tex << "\\documentclass[a4paper,10pt]{article}" << endl;
	tex << "\\nonstopmode" << endl;
	tex << "\\usepackage{amssymb}" << endl;
	tex << "\\usepackage{longtable}" << endl;
  tex << "\\usepackage{ifthen}" << endl;
  tex << "%\\usepackage{tikz}" << endl;
	tex << "\\usepackage{pgfplots}" << endl << endl;
  tex << "\\textwidth=16.1cm \\textheight=27.0cm \\topmargin=-1.8cm" << endl;
	tex << "\\oddsidemargin=0.1cm \\evensidemargin=0.1cm \\footskip=45pt" << endl;
	tex << endl;
	tex << "\\begin{document}" << endl;
	tex << endl;

  tex << "\\pgfplotsset{" << endl;
  tex << "/pgfplots/log number format basis/.code 2 args={" << endl;
  tex << "  \\ifdim #1 pt=2pt" << endl;
  tex << "    \\ifdim #2 pt<10pt" << endl;
  tex << "      \\pgfmathparse{#1^#2}" << endl;
  tex << "      \\pgfmathtruncatemacro\\r{\\pgfmathresult} \\r " << endl;
  tex << "    \\else" << endl;
  tex << "      \\ifdim #2 pt<20pt" << endl;
	tex << "        \\pgfmathparse{#1^(#2 - 10)}" << endl;
	tex << "        \\pgfmathprintnumber{\\pgfmathresult}K" << endl;
  tex << "      \\else" << endl;
	tex << "        \\ifdim #2 pt<30pt" << endl;
	tex << "          \\pgfmathparse{#1^(#2 - 20)}" << endl;
	tex << "          \\pgfmathprintnumber{\\pgfmathresult}M" << endl;
	tex << "        \\else" << endl;
	tex << "          \\ifdim #2 pt<40pt" << endl;
	tex << "            \\pgfmathparse{#1^(#2 - 30)}" << endl;
	tex << "            \\pgfmathprintnumber{\\pgfmathresult}G" << endl;
	tex << "          \\else" << endl;
	tex << "            \\ifdim #2 pt<50pt" << endl;
	tex << "              \\pgfmathparse{#1^(#2 - 40)}" << endl;
	tex << "              \\pgfmathprintnumber{\\pgfmathresult}T" << endl;
	tex << "            \\else" << endl;
	tex << "              >1P" << endl;
  tex << "            \\fi" << endl;
	tex << "          \\fi" << endl;
	tex << "        \\fi" << endl;
  tex << "      \\fi" << endl;
  tex << "    \\fi" << endl;
  tex << "  \\fi" << endl;
  tex << "  \\ifdim #1 pt=10pt" << endl;
  tex << "    $#1^{\\pgfmathprintnumber{#2}}$" << endl;
  tex << "  \\fi" << endl;
  tex << "}}" << endl << endl;
}

/*
 * Write the document footer.
 *
 * @param tex the latex file output stream
 */
static void write_footer(fstream& tex)
{
	tex << endl;
  tex << "\\end{document}" << endl;
}

/*
 * Write the trace properties page.
 *
 * @param tex the latex file output stream
 * @param alldata structure containing all the needed information
 */
static void write_traceProperties(fstream& tex, struct AllData& alldata)
{
  tex << "\\begin{titlepage}\\thispagestyle{empty}" << endl;
  tex << "\\begin{huge}\\begin{flushleft}\\bf{OTF Profile}"
         "\\end{flushleft}\\end{huge}" << endl;
  tex << "\\hrule" << endl;
  tex << "\\begin{flushright}\\textbf{\\large Trace Properties}"
         "\\end{flushright}" << endl;

  tex << "\\vspace{0.5\\baselineskip}" << endl;

  tex << "\\begin{flushleft}" << endl;
  tex << "\\begin{tabular}{ll}" << endl;
  tex << "\\bf{OTF Version:} & \\verb|" << alldata.version.c_str() << "| \\\\" << endl;
  tex << "\\bf{Creator:} & \\verb|" << alldata.creator << "|\\\\" << endl;

  /* parse the file path and write only the file's name */
  {
    string purFile = alldata.params.input_file_prefix;
    size_t found;

    found = alldata.params.input_file_prefix.rfind('/');
    
    if (found != string::npos){
      purFile = alldata.params.input_file_prefix.substr(found+1, string::npos);
    }

    tex << "\\bf{File:} & \\verb|" << purFile << ".otf|";
  }

  tex << endl << "\\end{tabular}" << endl << endl;

  tex << "\\vspace{1\\baselineskip}" << endl;

  tex << "\\begin{tabular}{ll}" << endl;
  tex << "\\bf{Number of Processes:} & \\verb|" 
      << alldata.allProcesses.size() << "|\\\\" << endl;

  {
    char unit; 
    string unitString; //unused
    uint64_t quant;
    
    quant = getScaleQuantifierLog10(alldata.timerResolution,
                                    alldata.timerResolution,
                                    unit, unitString);

    tex << "\\bf{Timer Resolution:} & \\verb|"
        << alldata.timerResolution/(double)quant << " " << unit << "Hz|" << endl;
  }

  tex << "\\end{tabular}" << endl << endl;

  tex << "\\vspace{1\\baselineskip}" << endl;

  if(!alldata.comments.empty()){
    tex << "\\begin{tabular}{l}\\bf{Comments:}\\end{tabular}" << endl;
    tex << "\\begin{quote}\\begin{verbatim}" << endl;
    tex << alldata.comments << endl;
    tex << "\\end{verbatim}\\end{quote}" << endl;
  }

  tex << "\\end{flushleft}" << endl;
  tex << "\\vspace*{\\fill}" << endl;
  tex << "\\begin{flushright}\\today\\end{flushright}" << endl;

  tex << "\\end{titlepage}" << endl <<endl;
	tex << "\\newpage" << endl << endl;
}

/*
 * Get an RGB color including gray.
 *
 * @param min the minimum value of the scale
 * @param max the maximum value of the scale
 * @param value the value, which should be colorized
 * @param red the red part of the color
 * @param green the green part of the color
 * @param blue the blue part of the color
 *
 * @return true if color encoding was successful.
 */
static bool get_color_gray(double min, double max, double value,
                   float& red, float& green, float& blue)
{
	if((value == min) || (min == max)){
		red = 0.9f; green = 0.9f; blue = 0.9f;
		return true;
	}

	if(value == max){
		red = 1.0; green = 0.0; blue = 0.0;
		return true;
	}

	double factor = (max - min) / 5.0;
	uint32_t part = (uint32_t) (((value - min) * 5.0) / (max - min));
	double min_temp = min + (factor * part);
	double max_temp = min + (factor * (part + 1.0));
	double part_temp;

	if(value == min_temp)
		part_temp = 0.0;
	else if(value == max_temp)
		part_temp = 1.0;
	else
		part_temp = (value - min_temp) / (max_temp - min_temp);
  
	if (part == 0)
		part_temp = part_temp / 2;

	switch(part){
		case 0 :
			red = (float) (0.9 - part_temp);
			green = (float) (0.9 - part_temp);
			blue = (float) (0.9 - part_temp);
			break;
		case 1 :
			red = (float) (0.0);
			green = (float) (part_temp);
			blue = (float) (1.0);
			break;
		case 2 :
			red = (float) (0.0);
			green = (float) (1.0);
			blue = (float) (1.0 - part_temp);
			break;
		case 3 :
			red = (float) (part_temp);
			green = (float) (1.0);
			blue = (float) (0.0);
			break;
		case 4 :
			red = (float) (1.0);
			green = (float) (1.0 - part_temp);
			blue = (float) (0.0);
			break;
		default :
      cerr << "Error in get_color(). Wrong part calculated." << endl;
      return false;
	}

	return true;
}

/*
 * Function ID as uint64_t
 * FunctionData (counterValue, exclTime, inclTime)
 * sorted by counter ID first, then function ID
 */
typedef std::map< uint64_t, FunctionData >::const_iterator itFunc_t;

/*
 * Write a latex function summary table to the given file stream.
 *
 * @param tex the given file stream (reference)
 * @param alldata the global data
 */
static void write_functionTable(fstream& tex, struct AllData& alldata)
{
  int mapSize = alldata.functionMapGlobal.size();
  int max = FUNC_TABLE_LEN;
  int count = 0;

  /* sort by exclusive time (key) into multimap */
  std::multimap<double, itFunc_t> sortedMap;

  {
    itFunc_t it = alldata.functionMapGlobal.begin();
    itFunc_t itend = alldata.functionMapGlobal.end();

    while(itend != it){
      /* only functions, which are at least invoked once */
      if(it->second.count.cnt){
        sortedMap.insert(
          pair<double, itFunc_t>((double)(it->second.excl_time.sum), it));
      }
      it++;
    }
  }
    
  /* write the table head */
  if(mapSize < max) max = mapSize;

  tex << "\\begin{center}\\small" << endl;
  tex << "{\\Large \\bf Top " << max << " of " << mapSize << " Functions}";
	tex << endl << "\\bigskip" << endl;
	tex << "\\begin{longtable}{|l||r|r|r|}" << endl << endl;
	tex << "   \\hline" << endl;
	tex << "   \\bf Function & \\bf invocations[\\#] & " <<
         "\\bf excl. time[sec] $\\nabla$ & \\bf incl. time[sec] \\\\" << endl;
	tex << "   \\hline\\hline" << endl;

  /* write the sorted function table */
  {
    std::multimap<double, itFunc_t>::const_reverse_iterator it =
      sortedMap.rbegin();
    std::multimap<double, itFunc_t>::const_reverse_iterator itend =
      sortedMap.rend();

    /* for a given max number of functions */
    while(itend != it){
      if(it->second->second.count.cnt) {
        string func_name = alldata.functionIdNameMap[it->second->first];
        if(func_name.size() > FUNC_NAME_MAX_LEN)
          func_name.resize(FUNC_NAME_MAX_LEN);
        tex << "  \\verb|" << func_name << "| & "
            << "  \\verb|" << it->second->second.count.cnt << "| & "
            << "  \\verb|" << it->second->second.excl_time.sum/alldata.timerResolution << "| & "
            << "  \\verb|" << it->second->second.incl_time.sum/alldata.timerResolution << "| \\\\" << endl;
      }
      it++;
      count++;

      /* stop after the given maximum number of functions */
      if(max == count) break;

      /* draw a horizontal line every 3 function entries */
      if((count % 3) == 0) tex << "      \\hline" << endl;
    }
  }
    

  tex << "   \\hline" << endl;
	tex << "\\end{longtable}" << endl << endl;
  tex << "\\end{center}" << endl;
	tex << "\\newpage" << endl << endl;

}

/*
 * Pair (counterID, functionID)
 * FunctionData (counterValue, exclTime, inclTime)
 * sorted by counter ID first, then function ID
 */
typedef std::map< Pair, FunctionData, ltPair >::const_iterator itCtr_t;
/* sorted by exclusive time */
typedef std::multimap<double, itCtr_t> sortedCtrMap_t;

/* 
 * Writes the sorted function table.
 * 
 * @param tex reference to the latex output stream
 * @param sortedMap reference to the sorted counter map
 * @param alldata the global data structure
 */
static void write_counterTable(fstream& tex, sortedCtrMap_t& sortedMap,
                               struct AllData& alldata)
{
  uint64_t timerRes = alldata.timerResolution;
  sortedCtrMap_t::const_reverse_iterator it = sortedMap.rbegin();
  sortedCtrMap_t::const_reverse_iterator itend = sortedMap.rend();
  string ctr_name = "";

  unsigned long int count = 0;
  unsigned long int max = CTR_TABLE_LEN;
  
  if(max > sortedMap.size()) max = sortedMap.size();

  if(itend != it){
    ctr_name = alldata.counterIdNameMap[it->second->first.a];
    if(ctr_name.size() > CTR_NAME_MAX_LEN)
      ctr_name.resize(CTR_NAME_MAX_LEN);
  }else return;

  /* get the minimum and maximum value to define the unit */
  double maxVal = 0;
  double minVal = numeric_limits<double>::max();
  while(it != itend){
    double currVal = it->second->second.excl_time.sum/it->first*timerRes;
    if(currVal < minVal) minVal = currVal;
    if(currVal > maxVal) maxVal = currVal;
    
    it++;
    count++;

    /* stop after the given maximum number of functions */
    if(max == count) break;
  }

  char unit = ' ';
  string unitL = "";
  uint64_t divisor = getScaleQuantifierLog10(minVal, maxVal, unit, unitL);

  //cout << "Divisor: " << divisor << " unit: " << string(&unit,1) << endl;

  tex << "\\begin{center}\\small" << endl;
  tex << "{\\large \\bf \\verb|" << ctr_name << "| [" << unitL << "] (Top "
      << max << ")}" << endl;
  tex << "\\begin{longtable}{|l||r|r|}" << endl;
  tex << "   \\hline" << endl;
  tex << "   \\bf Function & " << "\\bf excl. time[sec] & \\bf \\verb|"
                               << ctr_name << "/sec| \\\\" << endl;
  tex << "   \\hline\\hline" << endl;

  it = sortedMap.rbegin(); // reset iterator
  count = 0; // reset counter
  while(it != itend){
    string func_name = alldata.functionIdNameMap[it->second->first.b];
    if(func_name.size() > FUNC_NAME_MAX_LEN) func_name.resize(FUNC_NAME_MAX_LEN);

    tex <<
        "  \\verb|" << func_name << "| & " << // function name
        "  \\verb|" << it->first/timerRes << "| & " <<  // exclusive time
        "  \\verb|" << it->second->second.excl_time.sum/it->first*timerRes/divisor << "| \\\\" << endl;

    it++;
    count++;

    /* stop after the given maximum number of functions */
    if(max == count) break;

    /* draw a horizontal line every 3 function entries */
    if((count % 3) == 0) tex << "      \\hline" << endl;
  }

  tex << "   \\hline" << endl;
  tex << "\\end{longtable}" << endl << endl;
  tex << "\\end{center}" << endl;
  /*tex << "\\bigskip" << endl << endl;*/
  tex << "\\newpage" << endl << endl;
}

/*
 * Write the counter tables.
 * FunctionData.count ... value requests
 * FunctionData.excl_time ... exclusive counter values
 * FunctionData.incl_time ... inclusive counter values
 */
static void write_counterTables(fstream& tex, struct AllData& alldata)
{
  /*
   * source data map is sorted by counter ID, then function ID
   * precondition: sorted by counterID !!!
   */
  {
    /* separate into single maps, sorted by exclusive time */ 
    sortedCtrMap_t currMap;
    uint64_t lastCtrID;
    itCtr_t it = alldata.counterMapGlobal.begin();
    itCtr_t itend = alldata.counterMapGlobal.end();

    /* set the counter ID of the first entry */
    if(itend != it) lastCtrID = it->first.a;

    /* iterate the source map of counters (sorted by counter ID) */
    while(itend != it){
      /* insert only functions with counter values != 0 */
      if(it->second.count.cnt){
        itFunc_t itFuncMapEnd = alldata.functionMapGlobal.end();
        itFunc_t itFuncPos = alldata.functionMapGlobal.find(it->first.b);
        double excl_time = 0;

        /* check if function ID is in global function table */
        if(itFuncMapEnd != itFuncPos){
          excl_time = itFuncPos->second.excl_time.sum;
        }else
          cerr << "ERROR: Function ID not in global function table!" << endl;

        /* check for next counter ID */
        if(lastCtrID == it->first.a){
          currMap.insert(pair<double, itCtr_t>(excl_time, it));
        }else{
          /* create latex output for current counter */
          write_counterTable(tex, currMap, alldata);

          /* found next counter ID (source map is sorted by counter ID) */
          currMap.clear();
          
          currMap.insert(pair<double, itCtr_t>(excl_time, it));
        }

        /* set the last counter ID for next iteration */
        lastCtrID = it->first.a;
      }

      it++;
    }

    /* write last counter table */
    write_counterTable(tex, currMap, alldata);
  }

  tex << "\\newpage" << endl << endl;
}

/*
 * Write header of a ybar chart. This function decides about the y axis scaling
 * type (logarithmic basis 10 or 2 or linear scaling)
 *
 * @param tex the latex output stream
 * @param cclassType the collective class type (@see OTF_Definitions.h)
 * @param metricType the metric type
 * @param xLabels the x axis labels
 * @param minMax the min/max y values for this chart
 */
template <class type> static void write_ybarPlotHead(
                 fstream& tex, uint64_t cclassType, metric_t metricType,
                 vector<string> xLabels, MinMaxPair<type> minMax)
{
  tex << "\\begin{flushright}\\ttfamily\\small" << endl;
  tex << "\\begin{tikzpicture}" << endl;
  
  /* define an ymin variable */
  if(metricType == MSGLENGTH)
    tex << "\\def \\ymin {0.5}" << endl;

  /* TODO: quick hack to solve min=0 problem for logarithmic scaling */
  if(metricType == DURATION){
    double ymin = (double)minMax.min;
    double ymax = (double)minMax.max;

    /* is scaling logarithmic --> see if below (has to be the same!!!) */
    if(ymax - ymin > 100 || (ymax - ymin > 0.01 && ymax < 1)){
      if(ymin <= 0){
        tex << "\\def \\ymin {1e-1}" << endl;
      }
    }
  }

  tex << "\\begin{axis}[" << endl;
  tex << "  width=" << PLOT_WIDTH << "cm, height=" << PLOT_HEIGHT << "cm," << endl;
  tex << "  axis x line=bottom,x axis line style={-,line width=1pt}," << endl;
  tex << "  axis y line=left,y axis line style={-,line width=1pt}," << endl;
  tex << "  enlarge y limits={value=0.02,upper}," << endl;

  // @DEBUG
  // cout << "ymax=" << minMax.max << "; ymin=" << minMax.min << endl;

  /*** message length y axis settings ***/
  /* this works only for pgfplots since version 1.3 */
  // @TODO: ymin == 0, min - max line cannot be drawn
  if(metricType == MSGLENGTH){
    tex << "  ymode=log,log basis y=2,ymin=\\ymin," << endl;
  }

  /*** message duration y axis settings ***/
  if(metricType == DURATION){
    double ymin = (double)minMax.min;
    double ymax = (double)minMax.max;

    // @TODO: ymin == 0, min - max line cannot be drawn
    if(ymax - ymin > 100 || (ymax - ymin > 0.01 && ymax < 1)){
      // logarithmic mode
      tex << "  ymode=log,log basis y=10,";
      if(ymin <= 0){
        tex << "  ymin=1e-1," << endl;
      }else{
        tex << "  ymin=1e" << (int)floor(log10((double)ymin)) << "," << endl;
      }
    }else{
      // linear mode
      tex << "ymin=0," << endl;
      tex << "  ylabel style={at={(0,"
          << PLOT_HEIGHT-1 << "cm)},rotate=-90,anchor=north west}," << endl;
    }
  }

  /*** invocations y axis settings ***/
  if(metricType == INVOCATIONS){
    double ymax = (double)minMax.max;
    double ymin = (double)minMax.min;

    if(ymax > 1000 && ymin < 100 && ymin > 0){
      tex << "  ymode=log,log basis y=10,ymin=1e"
          << (int)floor(log10((double)ymin)) << ",";
    }else{
      tex << "  ymin=0,";
      //tex << "ytickmin={1}," << endl;
      if(0 < ymax && ymax < 10) 
        tex << "  ytick={0,...," << (uint64_t)ymax << "}," << endl;
    }
  }

  tex << "ymajorgrids,xminorgrids,minor x tick num=1," << endl;

  string title = ""; // the chart label
  string metric = ""; // the y label

  if(cclassType == OTF_COLLECTIVE_TYPE_UNKNOWN){
    title = "P2P";
  }else{
    collectiveId2String(cclassType, title);
  }

  switch(metricType){
    case INVOCATIONS:  
      title += " Invocations";
      metric = "";
      break;
    case DURATION:  
      title += " Duration";
      metric = "sec";
      break;
    case MSGLENGTH:  
      title += " Message Length";
      metric = "byte";
      break;
    default: break;
  }
  
  tex << "title=" << title;
  if(clustered) tex << " (average)" << endl;

  tex << ",ylabel={" << metric << "}," << endl;
  tex << "x tick label style={rotate=90,anchor=east,font=\\ttfamily\\footnotesize}," << endl;
  tex << "tick align=outside," << endl;
  tex << "tick style={line cap=round,line width=0.5pt,color=black," << endl;
	tex << "      major tick length=4pt,minor tick length=8pt}," << endl;
  tex << "major x tick style={line width=1, color=white}," << endl;
  tex << "scaled y ticks=true," << endl;
  tex << "bar width=" << YBAR_SIZE*2 << "pt," << endl;
  tex << "minor grid style={color=gray, line width=0.5pt, dashed}," << endl;
  tex << "xmin=-0.5," << endl;

  /* concerning the ticks */
  uint32_t xticks = xLabels.size();
  if(xticks == 0) xticks = xLabelNum;
  
  tex << "xmax=" << xticks-1 << ".5," << endl;
  tex << "xtick={0,...," << xticks-1 << "}," << endl;
  tex << "xticklabels={" << endl;
  for (unsigned int i = 0; i < xLabels.size(); i++){
    tex << xLabels[i] << ",";
  }

  tex << "},]" << endl;
}

/*
 * Write the footer for a ybar plot. The legend can be specified.
 *
 * @param tex the latex output file stream
 * @param legend create legend (1 send-receive; >1 no send receive)
 */
static void write_ybarPlotFoot(fstream& tex, uint8_t legend/* = 1*/)
{
  tex << "\\end{axis}" << endl;
  tex << "\\end{tikzpicture}" << endl << endl;

  tex << "\\end{flushright}" << endl;

  if(legend){
    tex << "\\begin{flushright}" << endl;
    tex << "\\bigskip" << endl;
    tex << "\\begin{tikzpicture}" << endl;
    
    /* create send, receive legend */
    if(legend == 1){
      tex << "\\node(a) at (0,0) [rectangle, draw, fill=" << COLOR_SEND << "] {};" << endl;
      tex << "\\node [black,right] at (a.east) {send};" << endl;
      tex << "\\node(b) at (2,0) [rectangle, draw, fill=" << COLOR_RECV << "] {};" << endl;
      tex << "\\node [black,right] at (b.east) {receive};" << endl;
    }

    if(clustered){
      tex << "\\draw[|-|,color=" << COLOR_MINMAX << ",line width=1pt] (4,-0.2) -- (4,0.2)" << endl;
      tex << "  node [right,xshift=2pt]{max}" << endl;
      tex << "  node [below right,yshift=-3pt,xshift=2pt]{ min};" << endl;
    }

    tex << "\\end{tikzpicture}" << endl;
    tex << "\\end{flushright}" << endl;
    tex << "\\newpage" << endl << endl;
  }
}

/*
 * Create the P2P chart plots.
 *
 * @param tex the latex output file stream
 * @param xLabels vector of the labels for the x axis
 * @param alldata data structure containing summarized profiling information
 *
 * @todo maybe use the pgftable method instead of string streams ...
 */
static void write_p2pAllPGFplots(fstream& tex, vector<string> xLabels,
                                 struct AllData& alldata)
{
  /* timer resolution */
  uint64_t tres = alldata.timerResolution;

  /* iterator over data map */
  std::map< uint64_t, MessageData >::const_iterator it = 
                                           alldata.messageMapPerCluster.begin();
  std::map< uint64_t, MessageData >::const_iterator itend = 
                                           alldata.messageMapPerCluster.end();

  /* counter for the x axis ticks */
  uint32_t i = 0;

  /* buffer for receive values */
  stringstream ss_count_recv (stringstream::in | stringstream::out);
  stringstream ss_bytes_recv (stringstream::in | stringstream::out);
  /*stringstream ss_duration_recv (stringstream::in | stringstream::out);*/

  /* buffer for remaining send values */
  stringstream ss_bytes_send (stringstream::in | stringstream::out);
  stringstream ss_duration_send (stringstream::in | stringstream::out);

  /* buffers for receive and min max values */
  stringstream ss_duration_send_e (stringstream::in | stringstream::out);
  /*stringstream ss_duration_recv_e (stringstream::in | stringstream::out);*/
  stringstream ss_bytes_send_e (stringstream::in | stringstream::out);
  stringstream ss_bytes_recv_e (stringstream::in | stringstream::out);

  /* plots containing duration available (old or new trace?) */
  bool byt_avail = false;
  bool dur_avail = false;

  /* return, if there are no messages available */
  if(alldata.messageMapPerCluster.empty()) return;

  /*
   *  Get min and max values to choose correct y axis scaling.
   */
  MinMaxMsgData minMax;
  while ( itend != it ) {
    
    if(clustered){

      /* invocations */
      if(it->second.count_send.cnt){
        uint64_t val = it->second.count_send.min;
        if(val < minMax.count.min) minMax.count.min = val;

        val = it->second.count_send.max;
        if(val > minMax.count.max) minMax.count.max = val;
      }

      if(it->second.count_recv.cnt){
        uint64_t val = it->second.count_recv.min;
        if(val < minMax.count.min) minMax.count.min = val;

        val = it->second.count_recv.max;
        if(val > minMax.count.max) minMax.count.max = val;
      }

    }else{
      /*** if processes are not clustered use the average values ***/

      /* invocations */
      if(it->second.count_send.cnt){
        uint64_t val = it->second.count_send.sum/it->second.count_send.cnt;

        if(val < minMax.count.min) minMax.count.min = val;
        if(val > minMax.count.max) minMax.count.max = val;
      }

      if(it->second.count_recv.cnt){
        uint64_t val = it->second.count_recv.sum/it->second.count_recv.cnt;

        if(val < minMax.count.min) minMax.count.min = val;
        if(val > minMax.count.max) minMax.count.max = val;
      }

      /* duration 
      if(it->second.duration_send.sum > 0 && it->second.duration_send.cnt){
        double val = it->second.duration_send.sum/it->second.duration_send.cnt/tres;

        if(val < minMax.duration.min) minMax.duration.min = val;
        if(val > minMax.duration.max) minMax.duration.max = val;

        dur_avail = true;
      }*/
    }

    it++;
  }
  
  /* reset iterator to write the data to file output */
  it = alldata.messageMapPerCluster.begin();

  write_ybarPlotHead(tex, OTF_COLLECTIVE_TYPE_UNKNOWN, INVOCATIONS,
                     vector<string>()/*xLabels*/, minMax.count);
  
  tex.precision(9);
  ss_count_recv.precision(9);
  ss_count_recv.setf(ios::floatfield);
  ss_bytes_recv.precision(9);
  ss_bytes_recv.setf(ios::floatfield);
  ss_bytes_send.precision(9);
  ss_bytes_send.setf(ios::floatfield);
  ss_duration_send.precision(9);
  ss_duration_send.setf(ios::floatfield);
  ss_duration_send_e.precision(9);
  ss_duration_send_e.setf(ios::floatfield);
  ss_bytes_send_e.precision(9);
  ss_bytes_send_e.setf(ios::floatfield);
  ss_bytes_recv_e.precision(9);
  ss_bytes_recv_e.setf(ios::floatfield);

  /* first of all write the average values */
  tex << "\\addplot[ybar, draw=black, mark=none, fill=" << COLOR_SEND 
      << ", xshift=-" << YBAR_SIZE << "]" << endl;
  tex << "  coordinates{" << endl;

  while ( itend != it ) {
    /*** send values ***/
    if(it->second.count_send.cnt){
      double val = (double)it->second.count_send.sum/it->second.count_send.cnt;
      
      /* directly write the send data */
      tex << "("<< i << "," << (double)val << ")";
    }

    /* buffer the remaining values */
    if(it->second.bytes_send.sum > 0 && it->second.bytes_send.cnt){
      ss_bytes_send << "("<< i << ","
              << (double)it->second.bytes_send.sum/it->second.bytes_send.cnt << ")";

      byt_avail = true;
    }

    if(it->second.duration_send.sum > 0 && it->second.duration_send.cnt){
      double val = it->second.duration_send.sum/it->second.duration_send.cnt/tres;
      ss_duration_send << "("<< i << "," << val << ")";

      /* set min/max values for duration */
      if(val < minMax.duration.min) minMax.duration.min = val;
      if(val > minMax.duration.max) minMax.duration.max = val;

      dur_avail = true;
    }

    /*** receive values ***/
    /* buffer the values for receive in string stream buffer */
    if(it->second.count_recv.cnt){
      double val = (double)it->second.count_recv.sum/it->second.count_recv.cnt;
      ss_count_recv << "("<< i << "," << (double)val << ")";
    }

    if(it->second.bytes_recv.sum > 0 && it->second.bytes_recv.cnt){
      ss_bytes_recv << "("<< i << ","
          << (double)it->second.bytes_recv.sum/it->second.bytes_recv.cnt << ")";

      byt_avail = true;
    }    

    it++;
    i++;
  }
  tex << "};" << endl;

  /* write receive count */
  tex << "\\addplot[ybar, draw=black, mark=none, fill=" << COLOR_RECV 
      << ", xshift=" << YBAR_SIZE << "]" << endl;
  tex << "  coordinates{" << endl;
  tex << ss_count_recv.str() << "};" << endl;

  /* check if min/max values shall be written */
  if(clustered){
    it = alldata.messageMapPerCluster.begin(); i = 0;
    while ( itend != it ) {

      /*** send invocations ***/
      if(it->second.count_send.cnt){
        uint64_t min = it->second.count_send.min;
        uint64_t max = it->second.count_send.max;

        /* write min/max values only if they differ */
        if(min != max){
          tex << "\\addplot[color=" << COLOR_MINMAX
              << ",mark=-,line width=1pt, xshift=-" << YBAR_SIZE << "]";
          tex << "  coordinates{" << endl;
          tex << "("<< i << "," << (double)min << ")";
          tex << "("<< i << "," << (double)max << ")";
          tex << "};" << endl;
        }
      }

      /*** receive invocations ***/
      if(it->second.count_recv.cnt){
        double min = (double)it->second.count_recv.min;
        double max = (double)it->second.count_recv.max;

        if(min < minMax.count.min) minMax.count.min = min;
        if(max > minMax.count.max) minMax.count.max = max;

        /* write min/max values only if they differ */
        if(min != max){
          tex << "\\addplot[color=" << COLOR_MINMAX << ",mark=-,line width=1pt, xshift=" << YBAR_SIZE << "]";
          tex << "  coordinates{" << endl;
          tex << "("<< i << "," << (double)min << ")";
          tex << "("<< i << "," << (double)max << ")";
          tex << "};" << endl;
        }
      }

      /*** send message length ***/
      if(byt_avail && it->second.bytes_send.cnt){
        double min = (double)it->second.bytes_send.min;
        double max = (double)it->second.bytes_send.max;
        
        if(min < minMax.bytes.min) minMax.bytes.min = min;
        if(max > minMax.bytes.max) minMax.bytes.max = max;

        /* write min/max values only if they differ */
        if(min != max){
          ss_bytes_send_e << "\\addplot[color=" << COLOR_MINMAX << ",mark=-,line width=1pt, xshift=-" << YBAR_SIZE << "]";
          ss_bytes_send_e << "  coordinates{" << endl;

          /* adapt zero min value for logarithmic scaling */
          if(min != 0){
            ss_bytes_send_e << "("<< i << "," << (double)min << ")";       
          }else ss_bytes_send_e << "("<< i << ",\\ymin)";

          ss_bytes_send_e << "("<< i << "," << (double)max << ")";
          ss_bytes_send_e << "};" << endl;
        }
      }

      /*** receive message length ***/
      if(byt_avail && it->second.bytes_recv.cnt){
        double min = (double)it->second.bytes_recv.min;
        double max = (double)it->second.bytes_recv.max;

        if(min < minMax.bytes.min) minMax.bytes.min = min;
        if(max > minMax.bytes.max) minMax.bytes.max = max;

        /* write min/max values only if they differ */
        if(min != max){
          ss_bytes_recv_e << "\\addplot[color=" << COLOR_MINMAX << ",mark=-,line width=1pt, xshift=" << YBAR_SIZE << "]";
          ss_bytes_recv_e << "  coordinates{" << endl;
          ss_bytes_recv_e << "("<< i << "," << (double)min << ")";
          ss_bytes_recv_e << "("<< i << "," << (double)max << ")";
          ss_bytes_recv_e << "};" << endl;
        }
      }

      /*** send duration ***/
      if(dur_avail && it->second.duration_send.cnt){
        double min = it->second.duration_send.min/tres;
        double max = it->second.duration_send.max/tres;

        if(min < minMax.duration.min) minMax.duration.min = min;
        if(max > minMax.duration.max) minMax.duration.max = max;

        /* write min/max values only if they differ */
        if(min != max){
          ss_duration_send_e << "\\addplot[color=" << COLOR_MINMAX << ",mark=-,line width=1pt]";
          ss_duration_send_e << "  coordinates{" << endl;
          ss_duration_send_e << "("<< i << "," << min << ")";
          ss_duration_send_e << "("<< i << "," << max << ")";
          ss_duration_send_e << "};" << endl;
        }
      }
      
      it++;
      i++;
    }

  }

  /* finish invocation chart */
  if(byt_avail || dur_avail)
    write_ybarPlotFoot(tex, 0);
  else
    write_ybarPlotFoot(tex, 1);

  /*** write message length chart ***/
  if(byt_avail){
    if(dur_avail){
      write_ybarPlotHead(tex, OTF_COLLECTIVE_TYPE_UNKNOWN, MSGLENGTH,
                         vector<string>(), minMax.bytes);
    }else{
      write_ybarPlotHead(tex, OTF_COLLECTIVE_TYPE_UNKNOWN, MSGLENGTH, xLabels,
                         minMax.bytes);
    }

    tex << "\\addplot[ybar, draw=black, mark=none, fill=" << COLOR_SEND
        << ", xshift=-" << YBAR_SIZE << "]" << endl;
    tex << "  coordinates{" << endl;
    tex << ss_bytes_send.str() << "};" << endl;

    tex << "\\addplot[ybar, draw=black, mark=none, fill=" << COLOR_RECV
        << ", xshift=" << YBAR_SIZE <<  "]" << endl;
    tex << "  coordinates{" << endl;
    tex << ss_bytes_recv.str() << "};" << endl;

    if(clustered) tex << ss_bytes_send_e.str() << ss_bytes_recv_e.str();

    if(dur_avail)
      write_ybarPlotFoot(tex, 0);
    else
      write_ybarPlotFoot(tex, 1);
  }

  /* 
   * Write message duration chart, if information are available.
   * No differentiation between send and receive.
   */
  if(dur_avail){
    write_ybarPlotHead(tex, OTF_COLLECTIVE_TYPE_UNKNOWN, DURATION, xLabels,
                       minMax.duration);

    tex << "\\addplot[ybar, draw=black, mark=none, fill=" << COLOR_SEND << "]" << endl;
    tex << "  coordinates{" << endl;
    tex << ss_duration_send.str() << "};" << endl;

    if(clustered) tex << ss_duration_send_e.str();

    write_ybarPlotFoot(tex, 1);
  }
  
  tex.precision(6);
}

/*
 * Creates a vector of the process or cluster labels.
 *
 * @param alldata structure containing all summarized profiling information
 * @param xLabels the resulting vector of process/cluster labels
 */
static void getXAxisLabels(struct AllData& alldata, vector<string>& xLabels)
{
  if(clustered){
    for(map<uint64_t, set<uint64_t> >::const_iterator it =
                                alldata.clustering.clustersToProcesses.begin();
         it != alldata.clustering.clustersToProcesses.end(); it++){

      /* map the first process id of the cluster to the process name */
      string procFrom = alldata.processIdNameMap[*(it->second.begin())];
      if(it->second.size() > 1){
        string procTo = alldata.processIdNameMap[*(it->second.rbegin())];
        procFrom = "\\shortstack[r]{"+procFrom+"\\\\"+"-\\\\"+procTo+"}";
      }

      /* add the created x axis label */
      xLabels.push_back(procFrom);
    }
  }else{
    for(set< Process, ltProcess >::const_iterator it=
        alldata.allProcesses.begin(); it != alldata.allProcesses.end(); it++) {
      xLabels.push_back( alldata.processIdNameMap[ it->process ] );
    }
  }
}

/*
 * Get process or cluster label from its ID.
 *
 * @param alldata structure containing all summarized profiling information
 * @param id the process or cluster id
 * @param label the label as string for the given ID
 */
static void getClusterLabel(struct AllData& alldata, uint64_t id,
                         string& label)
{
  if(clustered){
    set<uint64_t> procs = alldata.clustering.clustersToProcesses.find(id)->second;

    /* map the first process id of the cluster to the process name */
    label = alldata.processIdNameMap[*(procs.begin())];
    if(procs.size() > 1){
      string procTo = alldata.processIdNameMap[*(procs.rbegin())];
      label = label+"\\\\"+"-\\\\"+procTo;
    }
    
  }else{
    label = alldata.processIdNameMap[id];
  }
}

/*
 * Create the latex ouput for the P2P Message Rate Matrix.
 *
 * @param tex the latex output file stream
 * @param alldata structure containing all summarized profiling information
 */
static void write_p2pMsgRateMatrix(fstream& tex, struct AllData& alldata)
{
  std::map<Pair, MessageData, ltPair> msgMap = alldata.messageMapPerClusterPair;
  std::map<uint64_t,uint64_t> rankToPos;

  float scale = 0.7;
  uint32_t gridDim = 0;
  double minDataRate = 0;
  double maxDataRate = 0;
  uint64_t tres = alldata.timerResolution;


  /* check, if clustered to set the dimension of the matrix */
  if(clustered){
    gridDim = alldata.clustering.numClusters();
  }else{
    gridDim = alldata.allProcesses.size();
  }

  /* remove DEBUG output 
  cout << "gridDim:" << gridDim << " processes_num:" << alldata.allProcesses.size()
       << " cluster_num:" << alldata.clustering.numClusters() << endl; */


  std::map<Pair, MessageData, ltPair>::const_iterator it = msgMap.begin();
  std::map<Pair, MessageData, ltPair>::const_iterator itend = msgMap.end();
  
  if(it != itend){
    if(it->second.duration_send.sum == 0) return;
    minDataRate = it->second.bytes_send.sum/it->second.duration_send.sum*tres;
    maxDataRate = minDataRate;
  }else{
    return;
  }

  tex << "\\center{\\Large \\bf P2P - Message Rate (average)}" << endl;
  tex << "\\bigskip" << endl << endl;

  tex << "\\begin{center}" << endl;
  tex << "\\begin{tikzpicture} [step=1cm,scale=" << scale
      << ",every node/.style={scale=" << scale << "}]";
  if(clustered) tex << "\\small" << endl;

  /* preprocess data */
  uint64_t ctrInt = 0;
  while(it != itend){
    double tmp;
    
    /* get list of all ranks/clusters and map internal id for the grid position */

    /* check if already listed */
    /* TODO: vector[ctrInt]=rankID ??? */
    if(rankToPos.find(it->first.a) == rankToPos.end()){
      /* insert */
      rankToPos.insert(pair<uint64_t,uint64_t>(it->first.a, ctrInt));

      /* label the matrix */
      string label;
      getClusterLabel(alldata,it->first.a, label);
      tex << "\\node[anchor=east] at (0," << gridDim-ctrInt-1 << ".5)"
             " {\\shortstack[r]{" << label << "}};" << endl;
      tex << "\\node[anchor=west,rotate=90] at (" << ctrInt << ".5,"
          << gridDim << ") {\\shortstack[l]{" << label << "}};" << endl;

      //cout << "Process " << it->first.a << endl;
      ctrInt++;
    }

    /* get minimum and maximum data rate for color coding */
    if(it->second.bytes_send.cnt && it->second.duration_send.cnt && 
       (it->second.duration_send.sum > 0)){
      tmp = it->second.bytes_send.sum/it->second.duration_send.sum*tres;
      if(tmp > maxDataRate) maxDataRate = tmp;
      if(tmp < minDataRate) minDataRate = tmp;
    }

    /* ignore receive values */
    /*
    if(it->second.bytes_recv.cnt && it->second.duration_recv.cnt &&
       (it->second.duration_recv.sum > 0)){
      tmp = it->second.bytes_recv.sum/it->second.duration_recv.sum;
      if(tmp > maxDataRate) maxDataRate = tmp;
      if(tmp < minDataRate) minDataRate = tmp;
    }*/

    it++;
  }

  /* @DEBUG
  cout << "Processes found: " << ctrInt << " -- min: " << minDataRate
       << " max: " << maxDataRate << endl;*/

  /* Quantifier (K, M, G, T) for large values */
  uint8_t colorsteps = 20;
  char quant = ' ';
  uint64_t div = getScaleQuantifierLog2(minDataRate, maxDataRate, quant);
  string unit = string(&quant,1);
  unit.append("Byte/s");
  maxDataRate /= div;
  minDataRate /= div;

  //cout << "min: " << minDataRate << " max: " << maxDataRate << " in [" << unit << "]" << endl;

  makeNiceScaleTicks(minDataRate, maxDataRate, colorsteps);

  /* colorize the fields */
  it = msgMap.begin();
  while(it != itend){
    if(it->second.bytes_send.cnt && it->second.duration_send.cnt &&
       (it->second.duration_send.sum > 0)){
      uint64_t x = rankToPos.find(it->first.a)->second; //pos for rank
      uint64_t y = gridDim-1-rankToPos.find(it->first.b)->second; //pos for receiving peer
      float r,g,b;

      /* @DEBUG
      cout << " Process " << it->first.a << " to Peer " << it->first.b
           << " datarate: " << it->second.bytes_send.sum/it->second.duration_send.sum << endl;
      */
      
      if(0){ /* if clustered, how to get min max values??? */
        /* get maximum color */
        get_color_gray(minDataRate, maxDataRate,
            it->second.bytes_send.max/it->second.duration_send.max*tres/div, r, g, b);

        tex << "\\node[minimum size=1cm,anchor=south west] at ("
            << x << "," << y << ") [rectangle, fill={rgb,1:red,"
            << r << " ;green," << g  << ";blue," << b << "}] {};" << endl;

        /* get minimum color */
        get_color_gray(minDataRate, maxDataRate,
            it->second.bytes_send.min/it->second.duration_send.min*tres/div, r, g, b);

        tex << "\\node[minimum size=0.8cm,anchor=south west] at ("
            << x << ".1," << y << ".1) [rectangle, fill={rgb,1:red,"
            << r << " ;green," << g  << ";blue," << b << "}] {};" << endl;

        /* get average color */
        get_color_gray(minDataRate, maxDataRate,
            it->second.bytes_send.sum/it->second.duration_send.sum*tres/div, r, g, b);

        tex << "\\node[minimum size=0.6cm,anchor=south west] at ("
            << x << ".2," << y << ".2) [rectangle, fill={rgb,1:red,"
            << r << " ;green," << g  << ";blue," << b << "}] {};" << endl;
      }else{
        /* get average color */
        get_color_gray(minDataRate, maxDataRate,
            it->second.bytes_send.sum/it->second.duration_send.sum*tres/div, r, g, b);

        tex << "\\node[minimum size=1cm,anchor=south west] at ("
            << x << "," << y << ") [rectangle, fill={rgb,1:red,"
            << r << " ;green," << g  << ";blue," << b << "}] {};" << endl;
      }
    }
    
    it++;
  }

  tex << "\\draw[dotted] (-0.2,0) grid[step=1cm] ("
      << gridDim << "," << gridDim << ".2);" << endl;

  /* draw sender/receiver description */
  tex << "\\draw (0," << gridDim << ") -- (-1.2," << gridDim+1 << ".2) "
         "node [above right=-1, rotate=-45] {Receiver} "
         "node [below right, rotate=-45] {Sender};" << endl;

  tex << "\\end{tikzpicture}\\bigskip" << endl << endl;

  tex << "\\begin{tikzpicture} [step=1cm,scale=" << scale
      << ",every node/.style={scale=" << scale << "}]" << endl;

  /* draw the colormap legend */
  double interval = (maxDataRate - minDataRate)/(colorsteps);
  int i = 0;
  
  tex.setf(ios::fixed, ios::floatfield);
  tex.precision(4);
  for(;i < (int)colorsteps+1; i++){
    float r,g,b;
    double value = minDataRate+i*interval;

    get_color_gray(minDataRate, maxDataRate, value, r, g, b);

    /* color box */
    tex << "\\node[minimum size=0.95cm,anchor=south west] at ("
        << i-1 << ",-2) [rectangle, fill={rgb,1:red,"
        << r << " ;green," << g  << ";blue," << b << "}] {};" << endl;

    /* datarate description */
    tex << "\\node[anchor=east,rotate=90] at (" << i-0.5 << ",-2) {"
        << value << " "<< unit << "};" << endl;
  }

  tex << "\\end{tikzpicture}" << endl;
  tex << "\\end{center}" << endl;

  //@TODO: min-max-avg values
  
  /* write min-max-avg legend 
  float r,g,b;

  tex << endl << "\\bigskip" << endl;
  tex << "\\begin{center}" << endl;
  tex << "\\begin{tikzpicture} [step=1cm,scale=" << scale
    << ",every node/.style={scale=" << scale << "}]";
  tex << "\\footnotesize" << endl;

  get_color_gray(minDataRate, maxDataRate, maxDataRate, r, g, b);
  tex << "\\node(max)[minimum size=1cm,anchor=center] at (0,0) "
         "[rectangle, fill={rgb,1:red,"
      << r << ";green," << g << ";blue," << b << "}] {};" << endl;

  get_color_gray(minDataRate, maxDataRate, minDataRate, r, g, b);
  tex << "\\node(max)[minimum size=0.8cm,anchor=center] at (0,0)"
         "[rectangle, fill={rgb,1:red,"
      << r << ";green," << g << ";blue," << b << "}] {};" << endl;

  get_color_gray(minDataRate, maxDataRate, (maxDataRate+minDataRate)/2, r, g, b);
  tex << "\\node(max)[minimum size=0.6cm,anchor=center] at (0,0)"
         "[rectangle, fill={rgb,1:red,"
      << r << ";green," << g << ";blue," << b << "}] {};" << endl;

  tex << "\\node[anchor=west] at (2,0.4) {Maximum};" << endl;
  tex << "\\node[anchor=west] at (2,0) {Average};" << endl;
  tex << "\\node[anchor=west] at (2,-0.35) {Minimum};" << endl;
  tex << "\\draw[arrows=stealth-](0.45,0.4) -- (2,0.4);" << endl;
  tex << "\\draw[arrows=stealth-](0,0) -- (2,0);" << endl;
  tex << "\\draw[arrows=stealth-] (0.35,-0.35) -- (2,-0.35);" << endl;
  tex << "\\end{tikzpicture}" << endl;
  tex << "\\end{center}" << endl;*/

  tex << "\\newpage" << endl;
  tex.setf(ios::floatfield);
  tex.precision(6);
}

/*
 * Creates Plot information for a given collective class and a given metric.
 *
 * @param tex the latex output file stream
 * @param cclassType type of the collective class
 * @param metricType the metric type (needed to identify columns in data table)
 * @param xnum number of clusters with values (number of rows in data table)
 */
static void write_CollectiveClassMetricPlot(fstream& tex, uint64_t cclassType,
                                            metric_t metricType, int xnum)
{
  string classTitle = "";
  string metric = "";

  collectiveId2String(cclassType, classTitle);

  switch(metricType){
    case INVOCATIONS:  metric = "cnt"; break;
    case DURATION:  metric = "dur"; break;
    case MSGLENGTH:  metric = "byt"; break;
    default: break;
  }

  tex << "\\addplot[ybar, draw=black, mark=none, fill=" << COLOR_SEND;
  if(cclassType != OTF_COLLECTIVE_TYPE_BARRIER)
    tex << ", xshift=-" << YBAR_SIZE;
  tex << "] table[x=cluster,y=" << metric.c_str() << "Send] {\\"
      << classTitle.c_str() << "};" << endl;

  if(cclassType != OTF_COLLECTIVE_TYPE_BARRIER){
    tex << "\\addplot[ybar, draw=black, mark=none, fill=" << COLOR_RECV
        << ", xshift=" << YBAR_SIZE << "] "
           "table[x=cluster,y=" << metric.c_str() << "Recv] {\\"
        << classTitle.c_str() << "};" << endl;
  }

  if(clustered){
    /* read the min max values from the table */
    tex << "\\def \\min {0}" << endl;
    tex << "\\def \\max {0}" << endl;
    tex << "\\def \\xval {0}" << endl;

    for(int i = 0; i < xnum; i++){
      /*** send ***/
      tex << "\\pgfplotstablegetelem{" << i <<"}{" << metric.c_str()
          << "SendMin}\\of{\\" << classTitle.c_str() << "}" << endl;
      tex << "\\let\\min=\\pgfplotsretval" << endl;
      tex << "\\pgfplotstablegetelem{" << i <<"}{" << metric.c_str()
          << "SendMax}\\of{\\" << classTitle.c_str() << "}" << endl;
      tex << "\\let\\max=\\pgfplotsretval" << endl;

      /* write min/max values only, if they differ */
      tex << "\\ifthenelse{\\equal{\\min}{\\max}}{}{" << endl;
      tex << "  \\pgfplotstablegetelem{" << i <<"}{cluster}\\of{\\"
          << classTitle.c_str() << "}" << endl;
      tex << "  \\let\\xval=\\pgfplotsretval" << endl;

      if(metricType == DURATION || metricType == MSGLENGTH){
        /* check whether ymin is zero and set min to the plot's min */
        tex << "  \\ifdefined\\ymin" << endl;
        tex << "    \\ifthenelse{\\equal{\\min}{0}}{\\let\\min=\\ymin}{}" << endl;
        //tex << "    \\ifdim \\min pt=0pt \\let\\min=\\ymin \\fi" << endl;
        tex << "  \\fi" << endl;
      }

      tex <<   "  \\addplot[color=" << COLOR_MINMAX << ",mark=-,line width=1pt,";

      /* do not shift for BARRIER, as there is no send and receive */
      if(cclassType != OTF_COLLECTIVE_TYPE_BARRIER)
        tex << "xshift=-" << YBAR_SIZE;

      tex << "] coordinates {(\\xval,\\min) (\\xval,\\max)};" << endl;
      tex << "}" << endl;


      /*** receive (not for collective BARRIER) ***/
      if(cclassType != OTF_COLLECTIVE_TYPE_BARRIER){
        tex << "\\pgfplotstablegetelem{" << i <<"}{" << metric.c_str()
            << "RecvMin}\\of{\\" << classTitle.c_str() << "}" << endl;
        tex << "\\let\\min=\\pgfplotsretval" << endl;
        tex << "\\pgfplotstablegetelem{" << i <<"}{" << metric.c_str()
            << "RecvMax}\\of{\\" << classTitle.c_str() << "}" << endl;
        tex << "\\let\\max=\\pgfplotsretval" << endl;

        /* write min/max values only, if they differ */
        tex << "\\ifthenelse{\\equal{\\min}{\\max}}{}{" << endl;
        tex << "  \\pgfplotstablegetelem{" << i <<"}{cluster}\\of{\\"
            << classTitle.c_str() << "}" << endl;
        tex << "  \\let\\xval=\\pgfplotsretval" << endl;
        
      if(metricType == DURATION || metricType == MSGLENGTH){
        /* check whether ymin is zero and set min to the plot's min */
        tex << "  \\ifdefined\\ymin" << endl;
        tex << "    \\ifthenelse{\\equal{\\min}{0}}{\\let\\min=\\ymin}{}" << endl;
        //tex << "    \\ifdim \\min pt=0pt \\let\\min=\\ymin \\fi" << endl;
        tex << "  \\fi" << endl;
      }

        tex << "  \\addplot[color=" << COLOR_MINMAX << ",mark=-,line width=1pt,"
               "xshift=" << YBAR_SIZE << "] "
               "coordinates {(\\xval,\\min) (\\xval,\\max)};}" << endl;
      }
    }
  }
}

/* 
 * Create latex output for all metrics of a collective class.
 *
 * @param tex latex output stream
 * @param cclassType collective class type (see "OTF_Definitions.h")
 * @param xLabels vector of labels for the x axis (ascending)
 * @param xnum number of clusters
 * @param minMax the min/max values for all metrics of given collective class
 */
static void write_CollectiveClass(fstream& tex, uint64_t cclassType,
                                  vector<string> xLabels, int xnum,
                                  MinMaxMsgData minMax)
{
  /***** invocations *****/
  write_ybarPlotHead(tex, cclassType, INVOCATIONS, vector<string>(),
                     minMax.count);
  write_CollectiveClassMetricPlot(tex, cclassType, INVOCATIONS, xnum);
  write_ybarPlotFoot(tex, 0);
  /******************************/

  /***** message length *****/
  if(cclassType != OTF_COLLECTIVE_TYPE_BARRIER){
    write_ybarPlotHead(tex, cclassType, MSGLENGTH, vector<string>(),
                       minMax.bytes);
    write_CollectiveClassMetricPlot(tex, cclassType, MSGLENGTH, xnum);
    write_ybarPlotFoot(tex, 0);
  }
  /*****************************/

  /***** duration *****/
  write_ybarPlotHead(tex, cclassType, DURATION, xLabels, minMax.duration);

  write_CollectiveClassMetricPlot(tex, cclassType, DURATION, xnum);
  if(cclassType == OTF_COLLECTIVE_TYPE_BARRIER)
    write_ybarPlotFoot(tex, 2);
  else
    write_ybarPlotFoot(tex, 1);
}

/*
 * Create latex output for collective operations. Creates the data tables and
 * calls the chart plot routines.
 *
 * @param tex the latex output file stream
 * @param xLabels vector of the labels for the x axis
 * @param alldata data structure containing summarized profiling information
 */
static void write_collectives(fstream& tex, vector<string> xLabels,
                              struct AllData& alldata)
{
  /* iterator over data map */
  map<Pair, CollectiveData, ltPair>::const_iterator it = 
                                        alldata.collectiveMapPerCluster.begin();
  map<Pair, CollectiveData, ltPair>::const_iterator itend = 
                                          alldata.collectiveMapPerCluster.end();
  uint64_t currClass = OTF_COLLECTIVE_TYPE_UNKNOWN;

  string classTitle = "";

  /* create data table head for first collective class */
  if(it != itend){
    currClass = it->first.a;

    /* create pgfplots table head */
    tex << "\\pgfplotstableread{" << endl;
    tex << "cluster cntSend cntRecv bytSend bytRecv durSend durRecv";

    /* add min max markers, if clustered */
    if(clustered){
      tex << " cntSendMin cntSendMax cntRecvMin cntRecvMax bytSendMin bytSendMax"
             " bytRecvMin bytRecvMax durSendMin durSendMax durRecvMin durRecvMax";
    }
    tex << endl;
    
  }else{
    return;
  }


  int xCtr = 0;
  MinMaxMsgData minMax;
  /* for every collective class (see "OTF_Definitions.h") */
  for(it = alldata.collectiveMapPerCluster.begin(); it != itend; it++){
    /* map is sorted by collective class -- check if collective class changes */
    if(it->first.a != currClass){
      /* finish plot */
      collectiveId2String(currClass, classTitle);
      tex << "}\\" << classTitle.c_str() << endl;

      /* write the three metrics of the collective class */
      write_CollectiveClass(tex, currClass, xLabels, xCtr, minMax);

      /* reset for next collective class */
      minMax.initialize();
      xCtr = 0;

      /* set current collective class */
      currClass = it->first.a;

      /* create pgfplots table head */
      tex << "\\pgfplotstableread{" << endl;
      tex << "cluster cntSend cntRecv bytSend bytRecv durSend durRecv";

      /* add min max markers, if clustered */
      if(clustered){
        tex << " cntSendMin cntSendMax cntRecvMin cntRecvMax bytSendMin bytSendMax"
               " bytRecvMin bytRecvMax durSendMin durSendMax durRecvMin durRecvMax";
      }
      tex << endl;
    }
    
    /*** write data for the current cluster ***/
    xCtr++;

    tex << it->first.b-1;
    
    tex.precision(9);

    if(it->second.count_send.cnt){
      double val = (double)it->second.count_send.sum/it->second.count_send.cnt;
      
      tex << " " << (double)val;
      
      if(val < minMax.count.min) minMax.count.min = val;
      if(val > minMax.count.max) minMax.count.max = val;
    }else tex << " 0";

    if(it->second.count_recv.cnt){
      double val = (double)it->second.count_recv.sum/it->second.count_recv.cnt;
      tex << " " << val;
      if(val < minMax.count.min) minMax.count.min = val;
      if(val > minMax.count.max) minMax.count.max = val;
    }else tex << " 0";

    if(it->second.bytes_send.cnt){
      tex << " " << (double)it->second.bytes_send.sum/it->second.bytes_send.cnt;
    }else tex << " 0";

    if(it->second.bytes_recv.cnt){
      tex << " " << (double)it->second.bytes_recv.sum/it->second.bytes_recv.cnt;
    }else tex << " 0";

    if(it->second.duration_send.cnt){
      double val = it->second.duration_send.sum/it->second.duration_send.cnt/alldata.timerResolution;
      tex << " " << val;
      if(val < minMax.duration.min) minMax.duration.min = val;
      if(val > minMax.duration.max) minMax.duration.max = val;
    }else tex << " 0";

    if(it->second.duration_recv.cnt){
      double val = it->second.duration_recv.sum/it->second.duration_recv.cnt/alldata.timerResolution;
      tex << " " << val;
      if(val < minMax.duration.min) minMax.duration.min = val;
      if(val > minMax.duration.max) minMax.duration.max = val;
    }else tex << " 0";

    /*** if processes are clustered ***/
    if(clustered){
      /*** invocations ***/
      if(it->second.count_send.cnt){
        double val = (double)it->second.count_send.min;
        tex << " " << (double)val;
        if(val < minMax.count.min) minMax.count.min = val;

        val = (double)it->second.count_send.max;
        tex << " " << val;
        if(val > minMax.count.max) minMax.count.max = val;
      }else tex << " 0 0";

      if(it->second.count_recv.cnt){
        double val = (double)it->second.count_recv.min;
        tex << " " << val;
        if(val < minMax.count.min) minMax.count.min = val;

        val = (double)it->second.count_recv.max;
        tex << " " << val;
        if(val < minMax.count.max) minMax.count.max = val;
      }else tex << " 0 0";

      /*** message length ***/
      if(it->second.bytes_send.cnt){
        tex << " " << (double)it->second.bytes_send.min
            << " " << (double)it->second.bytes_send.max;
      }else tex << " 0 0";

      if(it->second.bytes_recv.cnt){
        tex << " " << (double)it->second.bytes_recv.min
            << " " << (double)it->second.bytes_recv.max;
      }else tex << " 0 0";

      /*** duration ***/
      if(it->second.duration_send.cnt){
        double val = it->second.duration_send.min/alldata.timerResolution;
        tex << " " << val;
        if(val < minMax.duration.min) minMax.duration.min = val;
        
        val = it->second.duration_send.max/alldata.timerResolution;
        tex << " " << val;
        if(val > minMax.duration.max) minMax.duration.max = val;
      }else tex << " 0 0";

      if(it->second.duration_recv.cnt){
        double val = it->second.duration_recv.min/alldata.timerResolution;
        tex << " " << val;
        if(val < minMax.duration.min) minMax.duration.min = val;

        val = it->second.duration_recv.max/alldata.timerResolution;
        tex << " " << val;
        if(val > minMax.duration.max) minMax.duration.max = val;
      }else tex << " 0 0";
    }

    tex << endl;
  }
  
  tex.precision(6);

  /* write end of last collective class table */
  collectiveId2String(currClass, classTitle);
  tex << "}\\" << classTitle.c_str() << endl;

  /* write the three metrics of the collective class */
  write_CollectiveClass(tex, currClass, xLabels, xCtr, minMax);
}

/*
 * Create the P2P Message Rate Histogram.
 *
 * @param tex the latex output file stream
 * @param alldata data structure caontaining all necessary information
 */
static void write_p2pMsgRateHist(fstream& tex, struct AllData& alldata)
{
  typedef map< Pair, MessageSpeedData, ltPair >::const_iterator msgRateHistMapIt;

  float scale = 0.5;

  msgRateHistMapIt it = alldata.messageSpeedMapPerLength.begin();
  msgRateHistMapIt itend = alldata.messageSpeedMapPerLength.end();

  if(it == itend) return;

  tex << "\\center{\\Large \\bf P2P - Message Rate Histogram}" << endl;
  tex << "\\bigskip" << endl << endl;

  tex << "\\begin{center}" << endl;
  tex << "\\begin{tikzpicture} [step=1cm,scale=" << scale
      << ",every node/.style={scale=" << scale << "}]" << "\\Large" << endl;

  /* get the minimum and maximum values */
  uint64_t maxMsgRate = 0;
  uint64_t minMsgRate = (uint64_t)-1;
  uint64_t maxMsgLen  = 0;
  uint64_t minMsgLen  = (uint64_t)-1;
  uint64_t maxCount   = 0;
  while(it != itend){
    if(it->first.a > maxMsgRate) maxMsgRate = it->first.a;
    if(it->first.a < minMsgRate) minMsgRate = it->first.a;
    if(it->first.b > maxMsgLen) maxMsgLen = it->first.b;
    if(it->first.b < minMsgLen) minMsgLen = it->first.b;
    if(it->second.count.cnt > maxCount) maxCount = it->second.count.cnt;
    
    it++;
  }
  it = alldata.messageSpeedMapPerLength.begin(); // reset iterator

  /* redefine the minimum and maximum message length */
  /* we can try to add some free fields, as it looks better :-) */
  const uint8_t x_tile_max = 24;
  uint8_t xTileFac = 1;
  uint8_t x_tile_used = maxMsgLen - minMsgLen + 1;
  if(x_tile_used <= x_tile_max){
    /* can we start with 1 byte as minimum, as maximum is not too high */
    if(maxMsgLen <= x_tile_max){
      minMsgLen = 1; // refers to 1 byte
    }else{
      uint8_t x_tile_free = x_tile_max - x_tile_used;
      if(minMsgLen - x_tile_free/2 > 0)
        minMsgLen = minMsgLen - x_tile_free/2;
      else
        minMsgLen = 1; // refers to 1 byte
    }

    x_tile_used = maxMsgLen - minMsgLen + 1;
    if(x_tile_used < x_tile_max){
      maxMsgLen += x_tile_max - x_tile_used;
    }
  }else{
    /***** not enough tiles to display all x values --> summarize ******/
    /* get the summarize factor - number of tiles put together, start with 2 */
    xTileFac = 2;
    minMsgLen = 1;
    while(maxMsgLen/xTileFac >= x_tile_max){
      xTileFac++;
    }
  }

  //cout << "xTileFac: " << (int)xTileFac << " minMsgLen: " << minMsgLen << " maxMsgLen: " << maxMsgLen << endl;

  /* create the colored fields */
  tex.precision(6);
  tex.setf(ios::fixed, ios::floatfield);
  while(it != itend){
    if(it->second.count.cnt){
      float r,g,b;
      
      get_color_gray(1, maxCount, it->second.count.cnt, r, g, b);
      tex << "\\node[minimum size=1cm,anchor=south west] at "
             "(" << (it->first.b-minMsgLen)/xTileFac << ","
                 << it->first.a-minMsgRate << ") ";
      tex << "[rectangle, fill={rgb,1:red," << r << ";green," << g << ";blue," << b << "}] {};" << endl;
    }
    it++;
  }
  tex.setf(ios::floatfield);

  // % draw x ticks and labels
  for(uint64_t i = 0; i <= (maxMsgLen-minMsgLen)/xTileFac+1; i++){
    tex << "\\draw (" << i << ",-0.1) -- (" << i << ",0) node[rotate=90,left] at ("
        << i << ",0) {"
        << convertBase2Exponent((minMsgLen+i-1)*xTileFac) << "};" << endl;
  }

  // % draw y ticks and labels
  for(uint64_t i = 0; i <= maxMsgRate-minMsgRate+1; i++){
    tex << "\\draw (-0.1," << i << ") -- (0," << i << ") node[anchor=east] "
           "at (0," << i << ") {" << convertBase2Exponent(minMsgRate+i-1) << "};" << endl;
  }

  // draw x axis
  tex << "\\draw (-0.1,0) -- (" << (maxMsgLen-minMsgLen+1)/xTileFac << ",0);" << endl;

  // draw y axis
  tex << "\\draw (0,-0.1) -- (0," << maxMsgRate-minMsgRate+1 << "); %y axis" << endl;

  // draw grid
  tex << "\\draw[dotted] (0,0) grid ("<< (maxMsgLen-minMsgLen+1)/xTileFac << ","
          << maxMsgRate-minMsgRate+1 << ");" << endl;

  // draw y axis description
  tex << "\\node[rotate=90] at (-2," << (maxMsgRate-minMsgRate)/2.0 << ") {rate [byte/sec]};" << endl;

  // draw x axis description
  tex << "\\node at (" << (maxMsgLen-minMsgLen+1)/xTileFac/2.0 << ", -1.5) {message length [byte]};" << endl;

  tex << "\\end{tikzpicture}\\bigskip" << endl << endl;
  tex << "\\begin{tikzpicture} [step=1cm,scale=" << scale
      << ",every node/.style={scale=" << scale << "}]" << "\\Large" << endl;

  /* draw the colormap legend */
  uint32_t colorsteps = 20; //
  if(colorsteps > maxCount) colorsteps = maxCount;

  tex << "\\node at (" << colorsteps/2.0 + 1 << ", 0) {Number of Invocations};" << endl;

  //cout << "maxCount: " << maxCount << " colorsteps: " << colorsteps << endl;

  double interval = maxCount/(double)colorsteps;
  for(int i = 1; i <= (int)colorsteps; i++){
    float r,g,b;

    get_color_gray(1, maxCount, i*interval, r, g, b);

    /* color box */
    tex.precision(6);
    tex.setf(ios::fixed, ios::floatfield);
    tex << "\\node[minimum size=0.95cm,anchor=south west] at ("
        << i << ",-1.5) [rectangle, fill={rgb,1:red,"
        << r << " ;green," << g  << ";blue," << b << "}] {};" << endl;

    /* number of invocation description */
    tex << "\\node[anchor=east,rotate=90] at (" << i+0.5 << ",-1.5) {";
    tex.precision(0);
    tex << i*interval << "};" << endl;
  }
  tex.precision(6);
  tex.setf(ios::floatfield);

  tex << "\\end{tikzpicture}" << endl;
  tex << "\\end{center}" << endl;
  tex << "\\newpage" << endl;
  
}

/*
 * Create the latex output.
 *
 * @param alldata data structure containing summarized profiling information
 */
bool CreateTex( AllData& alldata ) {

    bool error= false;

    VerbosePrint( alldata, 1, true, "producing LaTeX output\n" );

    string tex_file_name= alldata.params.output_file_prefix + ".tex";
    fstream tex_file;

    /* open output file */
    tex_file.open(tex_file_name.c_str(), ios::out | ios::trunc);
    if ( !tex_file.good() ) {
        cerr << "ERROR: Unable to open file '" << tex_file_name
             << "' for writing." << endl;
        return false;
    }

    tex_file.setf(/*ios::fixed, */ios::floatfield);
    tex_file.precision(6);

    clustered= alldata.clustering.enabled;

    /* write the document header (including the \begin{document} */
    write_header(tex_file);
    write_traceProperties(tex_file,alldata);

    /* write the function and counter tables */
    write_functionTable(tex_file, alldata);
    write_counterTables(tex_file, alldata);

    /* get the x axis labels */
    getXAxisLabels(alldata, xLabels);
    xLabelNum = xLabels.size();

    /* write P2P plots */
    write_p2pAllPGFplots(tex_file, xLabels, alldata);
    write_p2pMsgRateMatrix(tex_file, alldata);
    write_p2pMsgRateHist(tex_file, alldata);

    write_collectives(tex_file, xLabels, alldata);

    write_footer(tex_file);
    tex_file.close();

    VerbosePrint( alldata, 2, true, " created file: %s\n",
                  tex_file_name.c_str() );

#if defined(PDFTEX) && defined(HAVE_PGFPLOTS_1_4) && HAVE_PGFPLOTS_1_4
    /* create PDF file, if desired */
    if ( alldata.params.create_pdf ) {

        VerbosePrint( alldata, 1, true, "producing PDF output\n" );

        /* compose pdflatex command */
        ostringstream cmd;
        cmd << PDFTEX << " " << tex_file_name << " >/dev/null 2>&1";

        /* execute pdflatex command (two times) on TeX file */
        for ( uint8_t i = 0; i < 2; i++ ) {

            VerbosePrint( alldata, 2, true, " %srunning command: %s\n",
                          (0 == i) ? "" : "re-", cmd.str().c_str() );

            int rc= system( cmd.str().c_str() );
            if ( 0 != WEXITSTATUS( rc ) ) {

                cerr << "ERROR: Could not create PDF file from '"
                     << tex_file_name << "'." << endl;
                error= true;
                break;

            }

        }

    }
#endif /* PDFTEX && HAVE_PGFPLOTS_1_4 */

    return !error;
}
