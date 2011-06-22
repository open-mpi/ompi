/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifdef _SX
#	include <unistd.h>
#	include <sys/socket.h> /* <- this include is needed on NEC SX platforms for gethostname() */
#endif

#ifdef _WIN32
#	include <Winsock2.h>
#endif

#include "OTF_Platform.h"

#include "Prodtex.h"

void bar_data(fstream& out, double value[16][7], bool var, uint32_t proc_count)
{
	double help = 0;
	if(!var && (proc_count > 16))
	{
		out << "\\def\\bardataI" << endl;
		out << "{" << endl;
		for(int k = 0; k < 16; k++)
		{
			if(value[k][2] > 0)
			  out << (k + 0.25) << " " << (log(value[k][2])/log(2.0) + 1) << endl;
			if(value[k][5] > 0)
			  out << (k + 0.75) << " " << (log(value[k][5])/log(2.0) + 1) << endl;
		}
		out << "}" << endl;
	}
	out << "\\def\\bardataII" << endl;
	out << "{" << endl;
	for(int k = 0; k < 16; k++)
	{
		if(value[k][0] > 0)
		{
			help = value[k][0] / value[k][6];
			if(help < 1)
				out << (k + 0.3) << " " << help << endl;
			else
			  out << (k + 0.3) << " " << (log(help)/log(2.0) + 1) << endl;
		}
		if(value[k][3] > 0)
		{
			help = value[k][3] / value[k][6];
			if(help < 1)
				out << (k + 0.8) << " " << help << endl;
			else
			  out << (k + 0.8) << " " << (log(help)/log(2.0) + 1) << endl;
		}
	}
	out << "}" << endl;
	if(!var && (proc_count > 16))
	{
		out << "\\def\\bardataIII" << endl;
		out << "{" << endl;
		for(int k = 0; k < 16; k++)
		{
			if(value[k][1] > 0)
			  out << (k + 0.4) << " " << (log(value[k][1])/log(2.0) + 1) << endl;
			if(value[k][4] > 0)	
			  out << (k + 0.9) << " " << (log(value[k][4])/log(2.0) + 1) << endl;
		}
		out << "}" << endl;
	}
}

void bar_chart(uint64_t max, fstream& out, string proc_name1[16], string proc_name2[16], 
               uint32_t proc_count, string chart_name, bool byte , double value[16][7], 
               bool var, bool sum)
{
	uint64_t max_help = max;
	if(var && (proc_count > 16))
	{
		double help;
		for(int k = 0; k < 16; k++)
		{
			if((value[k][0] > 0) && (value[k][6] > 1))
			{
				
				help = value[k][0] / value[k][6];
				value[k][2] = (value[k][1] -( value[k][6] * help * help)) / (value[k][6] - 1.0);
				value[k][2] = sqrt(value[k][2]);
				max = MAXIMUM(max, max_help + (uint64_t) value[k][2]);
			}
			else
			{
				value[k][2] = 0;
			}
			
			if((value[k][3] > 0) && (value[k][6] > 1))
			{
				
				help = value[k][3] / value[k][6];
				value[k][5] = (value[k][4] - (value[k][6] * help * help)) / (value[k][6] - 1.0);
				value[k][5] = sqrt(value[k][5]);
				max = MAXIMUM(max, max_help + (uint64_t) value[k][5]);
			}
			else
			{
				value[k][5] = 0;
			}
		}
		
	}
	
	out << "{\\Large \\bf " << chart_name << "}" << endl;
	out << endl;
	out << "\\bigskip" << endl;
	out << "\\bigskip" << endl;
	out << "\\bigskip" << endl;
	out << endl;
	out << "\\begin{help}" << endl;
	out << "\\psset{xunit=1,yunit=0.5}" << endl;
	out << "\\begin{pspicture}(0,0)(16," << ((uint64_t) log((double)max)/log(2.0) + 2) << ")" << endl;
	out << "   \\psaxes[labels=no,Oy=-1,ysubticks=2,ylogBase=2,Dy=2,ytickwidth=1pt," << endl;
	out << "            ysubtickwidth=1pt,xticksize=-1 " << ((uint64_t) log((double)max)/log(2.0) + 2) 
      << ",yticksize=0 16,ysubticksize=1," << endl;
   out << "            yticklinestyle=dotted,ysubticklinestyle=dotted]{-}(0,0)(0,0)(16.1," 
      << ((uint64_t) log((double)max)/log(2.0) + 2) << ")" << endl;
   uint32_t count = 1;
	out << "   \\rput[r](-0.2,0){0}" << endl;
	uint64_t i;
	if(byte)
	{
		for(i = 1; i <= max<<1; i <<= 1)
		{
			out << "\\psline{-}(-0.15," << count << ")(0," << count << ")" << endl;
			if(i < KBYTE)
			{
				out << "   \\rput[r](-0.2," << count << "){" << i <<"}" << endl;
			}
			else if(i < MBYTE)
			{
				out << "   \\rput[r](-0.2," << count << "){" << (i / KBYTE) << "K}" << endl;
			}
			else if(i < GBYTE)
			{
				out << "   \\rput[r](-0.2," << count << "){" << (i / MBYTE) << "M}" << endl;
			}
			else
			{
				out << "   \\rput[r](-0.2," << count << "){" << (i / GBYTE) << "G}" << endl;
			}
			count += 1;
		}
	}
	else
	{
		for(i = 1; i <= max<<1; i <<= 1)
		{
			out << "\\psline{-}(-0.15," << count << ")(0," << count << ")" << endl;
			if(i < KILO)
			{
				out << "   \\rput[r](-0.2," << count << "){" << i <<"}" << endl;
			}
			else if(i < MEGA)
			{
				out << "   \\rput[r](-0.2," << count << "){" << (i / KILO) << "K}" << endl;
			}
			else if(i < GIGA)
			{
				out << "   \\rput[r](-0.2," << count << "){" << (i / MEGA) << "M}" << endl;
			}
			else
			{
				out << "   \\rput[r](-0.2," << count << "){" << (i / GIGA) << "G}" << endl;
			}
			count += 1;
		}
	}
   if(!var && (proc_count > 16))
   {
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.4," << endl;
		out << "       fillcolor=red,fillstyle=solid]{\\bardataI}" << endl;
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.3," << endl;
		out << "       fillcolor=green,fillstyle=solid]{\\bardataII}" << endl;
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.10," << endl;
		out << "       fillcolor=blue,fillstyle=solid]{\\bardataIII}" << endl;
	}
	else
	{
		out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.46," << endl;
		out << "       fillcolor=green,fillstyle=solid]{\\bardataII}" << endl;
	}
	
	if(var && (proc_count > 16))
	{
		double help, test;
		for(int k = 0; k < 16; k++)
		{
			if(value[k][0] > 0)
			{
				help = value[k][0] / value[k][6];
				out << "\\psframe[fillstyle=solid,fillcolor=lightgray](" << (k + 0.2) << ",";
				if(help < value[k][2])
					out << "0";
				else
				{		
					if((test = help - value[k][2]) < 1)
						out << test;
					else
					  out << log(test)/log(2.0) + 1;
				}
				out << ")(" << (k + 0.4) << ",";
				test = help + value[k][2];
				if(test < 1)
					out << test;
				else
				  out << log(test)/log(2.0) + 1;
				out << ")" << endl;
			}
			if(value[k][3] > 0)
			{
				help = value[k][3] / value[k][6];
				out << "\\psframe[fillstyle=solid,fillcolor=lightgray](" << (k + 0.7) << ",";
				if(help < value[k][5])
					out << "0";
				else
				{		
					if((test = help - value[k][5]) < 1)
						out << test;
					else
					  out << log(test)/log(2.0) + 1;
				}
				out << ")(" << (k + 0.9) << ",";
				test = help + value[k][5];
				if(test < 1)
					out << test;
				else
				  out << log(test)/log(2.0) + 1;
				out << ")" << endl;
			}
		}
	}
	
	for(int k = 0; k < 16; k++)
	{
		if(proc_name1[k].size() > 15)
		{
			proc_name1[k].resize(15);
		}
		if(proc_count <= 16)
		{
			out << "  \\rput[r]{90}(" << (k + 0.5) << ",-0.8){" << proc_name1[k] << "}" << endl;
			out << "  \\rput[t](" << (k + 0.2) << ",-0.2){s}" << endl;
			out << "  \\rput[t](" << (k + 0.8) << ",-0.2){r}" << endl;
		}
		else
		{
			if(proc_name2[k].size() > 15)
			{
				proc_name2[k].resize(15);
			}
			out << "  \\rput[t](" << (k + 0.2) << ",-0.2){s}" << endl;
			out << "  \\rput[t](" << (k + 0.8) << ",-0.2){r}" << endl;
			out << "  \\rput[t](" << (k + 1.2) << ",-0.2){s}" << endl;
			out << "  \\rput[t](" << (k + 1.8) << ",-0.2){r}" << endl;
			out << "  \\rput[r]{90}(" << (k + 0.2) << ",-0.8){" << proc_name1[k] << "}" << endl;
			out << "  \\rput[r]{90}(" << (k + 0.5) << ",-0.8){-}" << endl;
			out << "  \\rput[r]{90}(" << (k + 0.8) << ",-0.8){" << proc_name2[k] << "}" << endl;
			k++;
		}
	}
	
	out << "  \\rput[l](0,-7){s - Send}" << endl;
	out << "  \\rput[l](0,-7.7){r - Receive}" << endl;
	
	if(!sum) {
	if( (chart_name.find("ONE2ALL") < chart_name.size()) || (chart_name.find("ALL2ALL") < chart_name.size()) ) {
		string sub = "the root";
		if(chart_name.find("ALL2ALL") < chart_name.size()) sub = "every";
		out << "  \\rput[l](0,-11.0){In this chart one collective operation call means that " << sub << " process broadcasts only one message to the}" << endl;
		out << "  \\rput[l](0,-11.7){group and not allways one message to every group member. All processes of the group, the root process}" << endl;
		out << "  \\rput[l](0,-12.4){included, receive one message in total.}" << endl;
	}
	} else {
		if( (chart_name.find("ONE2ALL") < chart_name.size()) || (chart_name.find("ALL2ALL") < chart_name.size()) ) {
		string sub = "the root";
		if(chart_name.find("ALL2ALL") < chart_name.size()) sub = "every";
		out << "  \\rput[l](0,-11.0){In this chart one collective operation call means that " << sub << " process broadcasts one message to every}" << endl;
		out << "  \\rput[l](0,-11.7){group member and not only one to the group. All processes of the group, the root process included,}" << endl;
		out << "  \\rput[l](0,-12.4){receive one message in total.}" << endl;
	}
	}
	
	if(!var && (proc_count > 16))
	{
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=red,linecolor=black](3,-7){Y}" << endl;
		out << "  \\rput[l](3.2,-7.0){Maximum}" << endl;
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=green,linecolor=black](3,-7.7){Y}" << endl;
		out << "  \\rput[l](3.2,-7.7){Average}" << endl;
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=blue,linecolor=black](3,-8.4){Y}" << endl;
		out << "  \\rput[l](3.2,-8.4){Minimum}" << endl;
	}
	out << "\\end{pspicture}" << endl;
	out << "\\end{help}" << endl;
	out << "\\newpage" << endl;
}

/**************************************** tex_header ****************************************/

void tex_header(fstream& out, global_data* gd_ptr)
{
	char hostname[255];
	gethostname(hostname, sizeof(hostname));
	out << "\\documentclass[a4paper,10pt]{article}" << endl;
	out << "\\usepackage{amssymb}" << endl;
	out << "\\usepackage{longtable}" << endl;
	out << "\\usepackage{pstricks,pst-plot,pstricks-add}" << endl;
	out << "\\textwidth=16.0cm \\textheight=27.0cm \\topmargin=-1.8cm" << endl;
	out << "\\oddsidemargin=0.1cm \\evensidemargin=0.1cm \\footskip=45pt" << endl;
	out << endl;
	out << "\\newcommand{\\PstDrawNode}[7]{%" << endl;
  	out << "   \\definecolor{MyColor}{rgb}{#1,#2,#3}%" << endl;
  	out << "   \\fnode[framesize=#4 #5,fillstyle=solid,fillcolor=MyColor,linecolor=MyColor](#6,#7){Y}" << endl;
	out << "}" << endl;
	out << endl;
	out << "\\newcommand{\\Print}[4]{%" << endl;
   out << "   \\definecolor{MyColor}{rgb}{#1,#2,#3}%" << endl;
   out << "   \\textcolor{MyColor}{#4}" << endl;
	out << "}" << endl;
	out << endl;
	out << "\\newenvironment{help}{}{}" << endl;
	out << endl;
	out << "\\begin{document}" << endl;
	out << endl;
	out << "\\begin{flushleft}" << endl;
	out << endl;
	out << "file: \\verb|" << gd_ptr->filename << "| \\\\" << endl;
	out << endl;
	out << "\\bigskip" << endl;
	out << "\\fbox{\\parbox{6.0 cm}{created by : " << gd_ptr->creator << "\\\\" << endl;
	out << "                      OTF-Version: " << gd_ptr->version << "}}" << endl;
	//out << "                      Author: Denis Huenich \\\\" << endl;
	//out << "                      ZIH, TU Dresden}}" << endl;
	out << "\\end{flushleft}" << endl;
	out << "\\bigskip" << endl;
	out << "\\bigskip" << endl;
	out << endl;
	out << "\\begin{center}" << endl;
	out << endl;
}

/***************************************** tex_foot *****************************************/

void tex_foot(fstream& out)
{
	out << endl;
	out << "\\end{center}" << endl;
	out << endl;	out << "\\end{document}" << endl;
}

/*********************************** Function and Counter ***********************************/

int tex_func(fstream& out, global_data* gd_ptr, vector<string> counter_names, bool sum)
{
	typedef multimap<double,func_temp,greater<double> > TempMap;
	typedef map<uint32_t,count_temp> CountMap1;
	
	int min;
	int max = 0;

	string tmp;
	uint32_t stellen = 0;

	int i;
	int help;
	string func_name;
	string count_name;
	bool c_name;
	
	TempMap temp_map;
	CountMap1 count_map1;
	double ticks;
	double excl_time, incl_time;
	double rate;
	float red,green,blue;  // color values for rgb
	
	uint64_t min_invoc = (uint64_t) -1; 
	uint64_t max_invoc = 0;
	// 0 = excl_time, 1 = incl_time
	double min_time[2] = {(double)((uint64_t) -1), (double)((uint64_t) -1)}; 
	double max_time[2] = {0.0, 0.0};
	
	vector<uint32_t> func_vector;
	vector<uint32_t> count_vector;
	vector<uint32_t> count_name_vector;
	Function_Value f_value;
	Counter_Value c_value;
	
	gd_ptr->sum_container.get_Function_Def_Key(1, func_vector);
	gd_ptr->sum_container.get_Counter_Def_Key(1, count_vector);
	ticks = (double) gd_ptr->sum_container.get_ticks(1);
	if(ticks < 1.0)
	{	
		ticks = 1.0;
		cerr << "Error in tex_func. No ticks given for this trace." << endl;
	}
	
	/* collects information for Function and Counter */
	
	vector<uint32_t>::iterator it_vector = func_vector.begin();
	vector<uint32_t>::iterator it_c_vector;
	vector<uint32_t>::iterator it_c_name;
	CountMap1::iterator it_c_map1;
	TempMap::iterator it_map;
	TempMap::iterator it_map_page;
	
	if(!counter_names.empty())
	{
		it_c_vector = count_vector.begin();
		while(it_c_vector != count_vector.end())
		{
			string s(strdup(gd_ptr->sum_container.get_Counter_Def(1, *it_c_vector).get_name()));
			transform(s.begin(), s.end(), s.begin(), ::toupper);
			vector<string>::iterator help_v;
			help_v = counter_names.begin();
			while(help_v != counter_names.end())
			{
				if(s == *help_v)
				{
					count_name_vector.push_back(*it_c_vector);
					break;	
				}
				++help_v;
			}
			++it_c_vector;
		}
	}
	while(it_vector != func_vector.end())
	{
		func_temp f_temp;
		f_value = gd_ptr->sum_container.get_Function(1, *it_vector, 0);
		if(f_value.get_invoc() > 0)
		{
			excl_time = (double) f_value.get_excl_time() / ticks;
			incl_time = (double) f_value.get_incl_time() / ticks;
			f_temp.func_id = *it_vector;
			f_temp.invoc = f_value.get_invoc();
			f_temp.incl_time = incl_time;
		
			it_c_vector = count_vector.begin();
			while(it_c_vector != count_vector.end())
			{
				c_name = false;
				c_value = gd_ptr->sum_container.get_Counter(1, *it_vector, 0, *it_c_vector);
				if(c_value.get_valid() == VALID)
				{
					if(count_name_vector.empty())
					{
						if((excl_time == 0.0) || (c_value.get_excl_value() == 0))
							rate = 0.0;
						else
							rate = ((double) c_value.get_excl_value()) / excl_time / MEGA;
					}
					else
					{
						it_c_name = count_name_vector.begin();
						while(it_c_name != count_name_vector.end())
						{
							if(*it_c_name == *it_c_vector)
							{
								c_name = true;
								break;	
							}
							++it_c_name;
						}
						if(c_name)
						{
							if(c_value.get_excl_value() == 0)
								rate = 0.0;
							else
								rate = ((double) c_value.get_excl_value()) / MEGA;
						}
						else
						{
							if((excl_time == 0.0) || (c_value.get_excl_value() == 0))
								rate = 0.0;
							else
								rate = ((double) c_value.get_excl_value()) / excl_time / MEGA;
						}
					}
					f_temp.count_map2[*it_c_vector] = rate;
				
					it_c_map1 = count_map1.find(*it_c_vector);
					if(it_c_map1 == count_map1.end())
					{
						count_temp c_temp;
						c_temp.min = rate;
						c_temp.max = rate;
						count_map1.insert(pair<uint32_t,count_temp>(*it_c_vector,c_temp));
					}
					else
					{
						if(it_c_map1->second.min == 0.0)
							it_c_map1->second.min = rate;
						else if(rate > 0.0)
							it_c_map1->second.min = MINIMUM(rate,it_c_map1->second.min);
						it_c_map1->second.max = MAXIMUM(rate,it_c_map1->second.max);
					}
				}
				++it_c_vector;
			}
			temp_map.insert(pair<const double,func_temp>(excl_time, f_temp));
		
			min_invoc = MINIMUM(f_value.get_invoc(), min_invoc);
			max_invoc = MAXIMUM(f_value.get_invoc(), max_invoc);
			min_time[0] = MINIMUM(excl_time, min_time[0]);
			max_time[0] = MAXIMUM(excl_time, max_time[0]);
			min_time[1] = MINIMUM(incl_time, min_time[1]);
			max_time[1] = MAXIMUM(incl_time, max_time[1]);
		}
		++it_vector;
	}

	/********************************** Top 50 of Function **********************************/
	/* How many functions in total*/

	for(it_map = temp_map.begin(); it_map!=temp_map.end(); ++it_map) {
	  max++;
	  tmp.assign(gd_ptr->sum_container.get_Function_Def(1, it_map->second.func_id).get_name());
	  if(tmp.size() > stellen) {
	    if(tmp.size() <= 20) stellen = (uint32_t) tmp.size(); else stellen = 20;
	  }
	}

	min = gd_ptr->TOP_FUNC;
	if(max < gd_ptr->TOP_FUNC) {
	  min = max;
	}

	it_map = temp_map.begin();

	out << "{\\Large \\bf Top " << min << " of " << max << " Functions}" << endl;
	out << endl;
	out << "\\bigskip" << endl; 
	out << endl;
	out << "\\begin{longtable}{|l||r|r|r|}" << endl;
	out << endl;
	out << "   \\hline" << endl;
	out << "   \\bf Function & \\bf invocations[\\#] & \\bf excl. time[sec] $\\nabla$ & " 
	   << "   \\bf incl. time[sec] \\\\" << endl;
	out << "   \\hline\\hline" << endl;
	for(i = 1; i <= min; i++)
	{
		func_name.assign(gd_ptr->sum_container.get_Function_Def(1, it_map->second.func_id).get_name());
		//if(func_name.size() > 20)
			func_name.resize(stellen,32);
			
		out << "      \\verb|" << func_name << "| & ";
		out.precision(2);
		gd_ptr->sum_container.get_color(min_invoc, max_invoc, it_map->second.invoc, 
		                                red, green, blue);
		out << "\\Print{" << red << "}{" << green << "}{" << blue << "}{" 
			<< it_map->second.invoc << "} & ";
		gd_ptr->sum_container.get_color(min_time[0], max_time[0], it_map->first, 
		                                red, green, blue);
		out << "\\Print{" << red << "}{" << green << "}{" << blue << "}{"; 
		out.precision(6);
		out	<< it_map->first << "} & ";
		out.precision(2);
		gd_ptr->sum_container.get_color(min_time[1], max_time[1], it_map->second.incl_time, 
		                                red, green, blue);
		out << "\\Print{" << red << "}{" << green << "}{" << blue << "}{"; 
		out.precision(6);
		out	<< it_map->second.incl_time << "} \\\\" << endl;
		
		if((i % 3) == 0)
			out << "      \\hline" << endl;
		
		++it_map;
		
		if(it_map == temp_map.end())
			break;
	}
	out << "   \\hline" << endl;
	out << "\\end{longtable}" << endl;
	out << endl;
	out << "\\newpage" << endl;
	out << endl;

	/********************************** Top 50 of Counter **********************************/

	CountMap1::iterator it_c_tmp_map;
	
	if(!sum) {
	
	  it_c_map1 = count_map1.begin();
	  CountMap2::iterator it_c_map2;
	  int page = 1;

	  while(it_c_map1 != count_map1.end())
	  {
		page = 1;
		it_map_page = temp_map.begin();
		it_map = it_map_page;

		while(page <= gd_ptr->TOP_FUNC && it_map != temp_map.end())
		{
			out << "{\\Large \\bf Top " << gd_ptr->TOP_FUNC << " of Counter [in Mega]}" << endl;
			out << endl;
			out << "\\bigskip" << endl; 
			out << endl;
			out << "\\begin{tabular}{|l||r|}" << endl;
			out << endl;
			out << "   \\hline" << endl;
			out << "   \\bf Function & \\bf excl. time[sec] \\\\" << endl;
			out << "   \\hline\\hline" << endl;
			
			it_map = it_map_page;
			for(i = page; i <= gd_ptr->TOP_FUNC; i++)
			{
				func_name.assign(gd_ptr->sum_container.get_Function_Def(1, it_map->second.func_id).get_name());
				//if(func_name.size() > 20)
					func_name.resize(stellen,32);
				out << "      \\verb|" << func_name << "| & " << endl;
				gd_ptr->sum_container.get_color(min_time[0], max_time[0], it_map->first, 
													red, green, blue);
				out << "\\Print{" << red << "}{" << green << "}{" << blue << "}{"; 
				out.precision(6);
				out	<< it_map->first << "}\\\\" << endl;
				if((i % 3) == 0)
					out << "      \\hline" << endl;
				++it_map;
				if((it_map == temp_map.end()) || ((i % 54) == 0))
					break;
			}
			out << "   \\hline" << endl;
			out << "\\end{tabular}" << endl;
			
			it_map = it_map_page;
			help = 1;
			i = page;
			while((help <= 2) && (it_c_map1 != count_map1.end()))
			{
				if((i % 54) == 1)
				{
					count_name.assign(gd_ptr->sum_container.get_Counter_Def(1, it_c_map1->first).get_name());
					if(count_name.size() > 20)
						count_name.resize(20);
					out << "\\begin{tabular}{r|}" << endl;
					out << endl;
					out << "   \\hline" << endl;
					
					c_name = false;
					if(count_name_vector.empty())
					{
						out << "   \\bf\\verb|" << count_name << " #/sec| \\\\" << endl;	
					}
					else
					{
						it_c_name = count_name_vector.begin();
						while(it_c_name != count_name_vector.end())
						{
							if(*it_c_name == it_c_map1->first)
							{
								c_name = true;
								break;	
							}
							++it_c_name;
						}
						if(c_name)
						{
							out << "   \\bf\\verb|" << count_name << " #| \\\\" << endl;
						}
						else
						{
							out << "   \\bf\\verb|" << count_name << " #/sec| \\\\" << endl;
						}
					}
					out << "   \\hline\\hline" << endl;
				}
				
				it_c_map2 = it_map->second.count_map2.find(it_c_map1->first);
				if((it_c_map2 == it_map->second.count_map2.end()) || (it_c_map2->second == 0.0))
				{
					out << "    no value\\\\" << endl;
				}
				else
				{
					out.precision(2);
					gd_ptr->sum_container.get_color(it_c_map1->second.min, it_c_map1->second.max,
						it_c_map2->second, red, green, blue);
					out << "    \\Print{" << red << "}{" << green << "}{" << blue << "}{";
					out.precision(6); 
					out	<< it_c_map2->second << "}\\\\" << endl;
				}
				
				if((i % 3) == 0)
					out << "      \\hline" << endl;
					
				++it_map;
				++i;
					
				if((it_map == temp_map.end()) || (i > gd_ptr->TOP_FUNC) || ((i % 54) == 1))
				{	
					++help;
					
					out << "   \\hline" << endl;
					out << "\\end{tabular}" << endl;
					it_c_tmp_map = count_map1.end();
					--it_c_tmp_map;
					if((help > 2) || (it_c_map1 == it_c_tmp_map))
					{
						it_map_page = it_map;
						page = i;
						out << "\\newpage" << endl;
						out << endl;
						if((i % 54) == 1) {
							if(it_c_map1 != it_c_tmp_map) {
							  --it_c_map1;
							}
							help = 1;
							break;
							}
						else {
							++it_c_map1;}
					}
					else
					{
						it_map = it_map_page;
						i = page;
						++it_c_map1;
					}
				}
			}
		}
		out << "\\newpage" << endl;
		out << endl;
	  }
	}

	return 0;
}

/******************************************* P2P *******************************************/

 /************************************ tex_p2p_values ************************************/

int tex_p2p_values(vector<uint32_t> proc_vector, fstream& out, global_data* gd_ptr, 
                   double ticks, uint32_t range, int type)
{
	P2P_Value p2p_value;
	float red,green,blue;  // color values for rgb
	double min = (double)((uint64_t) - 1);
	double max = 0.0;
	double min_var = (double)((uint64_t) - 1);
	double max_var = 0.0;
	double value = 0.0;
	double value_field[16][16][5];// 0 = value, 1 = min_value_local, 2 = max_value_local, 3 = count, 4 = value²
	string proc_name1[16];
	string proc_name2[16];
	
	for(int j = 0; j < 16; j++)
	{
		proc_name1[j].assign(" ");
		proc_name2[j].assign(" ");
		for(int k = 0; k < 16; k++)
		{
			for(int l = 0; l < 5; l++)
			{
				if(l == 1)	
					value_field[j][k][l] = (double)((uint64_t) - 1);
				else
					value_field[j][k][l] = 0.0;
			}
		}
	}
	vector<uint32_t>::iterator it_vector = proc_vector.begin();	
	vector<uint32_t>::iterator it_vector2;
	int count1 = 16; // 15 is the first process group in value field and 0 is the last
	int count2 = 16; // 15 is the first process group in value field and 0 is the last
	double j = 0.0;
	double k = 0.0;
	int help1 = (int) proc_vector.size();
	int help2;

	while(it_vector != proc_vector.end())
	{
		for(j = 0.0; j < ((double) help1 / (double) count1); j++)
		{
			if(j == 0.0)
				proc_name1[16 - count1].assign(gd_ptr->sum_container.get_Process_Def(1, *it_vector));
			it_vector2 = proc_vector.begin();
			count2 = 16;
			help2 = (int) proc_vector.size();

			while(it_vector2 != proc_vector.end())
			{
				for(k = 0.0; k < ((double) help2 / (double) count2); k++)
				{

					p2p_value = gd_ptr->sum_container.get_P2P(1, *it_vector, *it_vector2, 0, 0);
					if((p2p_value.get_time() > 0) && (p2p_value.get_length() > 0))
					{
						switch(type)
						{
							case P2P_AV_RAT  : value =  (double) p2p_value.get_length() / 
							                           ((double) p2p_value.get_time() / ticks);break;
							case P2P_AV_DUR  : value = ((double) p2p_value.get_time() / 
							                            (double) p2p_value.get_invoc()) / ticks;break;
							case P2P_AV_LEN  : value =  (double) p2p_value.get_length() / 
							                            (double) p2p_value.get_invoc();break;
							case P2P_SUM_DUR : value =  (double) p2p_value.get_time() / ticks;break;
							case P2P_SUM_LEN : value =  (double) p2p_value.get_length();break;
							default          : cerr << "Error in tex_p2p_values().Wrong type." 
							                        << endl;
							                   return 1;
						}
					}
					else
						value = 0.0;
					value_field[16 - count1][16 - count2][3] += 1.0;
					value_field[16 - count1][16 - count2][0] += value;
					if(value_field[16 - count1][16 - count2][0] > 0.0)
					{
						value_field[16 - count1][16 - count2][1] = MINIMUM(value_field[16 - count1][16 - count2][1], value);
						value_field[16 - count1][16 - count2][2] = MAXIMUM(value_field[16 - count1][16 - count2][2], value);
					
						if(gd_ptr->var)
						{
							value_field[16 - count1][16 - count2][4] += value * value;
						}
						min = MINIMUM(min, value);
						max = MAXIMUM(max, value);
					}
					
					++it_vector2;
				}
				--it_vector2;
				proc_name2[16 - count2].assign(gd_ptr->sum_container.get_Process_Def(1, *it_vector2));
				++it_vector2;
				help2 -= (int) k;
				if(count2 < 1)
					cerr << "Error in tex_p2p(). Wrong count2 value." << endl;
				--count2;
			}
			
			++it_vector;
		}
		help1 -= (int) j;
		if(count1 < 1)
			cerr << "Error in tex_p2p(). Wrong count1 value." << endl;
		--count1;
	}
	
	switch(type)
	{
		case P2P_AV_RAT  : out << "{\\Large \\bf P2P - Message Rate (average)}" << endl;break;
		case P2P_AV_DUR  : out << "{\\Large \\bf P2P - Message Duration (average)}" << endl;break;
		case P2P_AV_LEN  : out << "{\\Large \\bf P2P - Message Length (average)}" << endl;break;
		case P2P_SUM_DUR : out << "{\\Large \\bf P2P - Message Duration (sum)}" << endl;break;
		case P2P_SUM_LEN : out << "{\\Large \\bf P2P - Message Length (sum)}" << endl;break;
		default          : cerr << "Error in tex_p2p_values.Wrong type." << endl; return 1;
	}
	out << "\\bigskip" << endl; 
	out << "\\bigskip" << endl;
	if(max <= 0.0)
	{
		out << "no values" << endl;
		out << "\\newpage" << endl;
		out << endl;
		
		return 0;
	}
	out << "\\bigskip" << endl; 
	out << "\\bigskip" << endl;
	out << "\\bigskip" << endl;
	out << endl;
	out << "\\begin{help}" << endl;
	out << "\\psset{xunit=0.75,yunit=0.75}" << endl;
	out << "\\begin{pspicture}(0,-3)(16,13)" << endl;
	out << "   \\psgrid[subgriddiv=0,%" << endl;
  	out << "      griddots=5,%" << endl;
  	out << "      gridlabels=0](0,-3)(16,13)" << endl;
  	
  	if(gd_ptr->var && (proc_vector.size() > 16))
	{
		min = (double)((uint64_t) - 1);
		max = 0;
		for(int j = 0; j < 16; j++)
		{
			for(int k = 0; k < 16; k++)
			{
				if(value_field[j][k][0] > 0.0)
				{
					value_field[j][k][0] = value_field[j][k][0] / value_field[j][k][3];
					min = MINIMUM(min,value_field[j][k][0]);
					max = MAXIMUM(max,value_field[j][k][0]);
				
					if(value_field[j][k][3] == 1.0)
					{
						value_field[j][k][4] = 0;
					}
					else
					{
						value_field[j][k][4] = (value_field[j][k][4] - (value_field[j][k][3] * 
									              value_field[j][k][0] * value_field[j][k][0])) /
									              (value_field[j][k][3] - 1.0);
						value_field[j][k][4] = sqrt(value_field[j][k][4]);
					}
					min_var = MINIMUM(min_var, value_field[j][k][4]);
					max_var = MAXIMUM(max_var, value_field[j][k][4]);
				}
			}
		}
  	}
  	for(int l = 0; l < 16; l++)
	{
		for(int m = 0; m < 16; m++)
		{
			if(value_field[l][m][0] > 0.0)
			{
				if(gd_ptr->var && (proc_vector.size() > 16))
				{	
					if(!gd_ptr->vis)
					{
						gd_ptr->sum_container.get_gray(min_var, max_var, value_field[l][m][4], red, green, blue);
						out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
							<< "{0.73}{0.73}{" << (m + 0.5) << "}{" 
							<< (12 - l + 0.5) << "}" << endl;
						gd_ptr->sum_container.get_color(min, max, value_field[l][m][0], red, green, blue);
						out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
							<< "{0.5}{0.5}{" << (m + 0.5) << "}{" 
							<< (12 - l + 0.5) << "}" << endl;
					}
					else
					{
						gd_ptr->sum_container.get_color(min, max, value_field[l][m][0], red, green, blue);
						out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
							<< "{0.73}{0.73}{" << (m + 0.5) << "}{" 
							<< (12 - l + 0.5) << "}" << endl;
						gd_ptr->sum_container.get_gray(min_var, max_var, value_field[l][m][4], red, green, blue);
						out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
							<< "{0.25}{0.5}{" << (m + 0.75) << "}{" 
							<< (12 - l + 0.5) << "}" << endl;		
					}
				}
				else if(!gd_ptr->vis)
				{
					gd_ptr->sum_container.get_color(min, max, value_field[l][m][2], red, green, blue);
					out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
						<< "{0.73}{0.73}{" << (m + 0.5) << "}{" 
						<< (12 - l + 0.5) << "}" << endl;
					if(proc_vector.size() > 16)
					{
						gd_ptr->sum_container.get_color(min, max, value_field[l][m][1], red, green, blue);
						out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
							<< "{0.58}{0.58}{" << (m + 0.5) << "}{" 
							<< (12 - l + 0.5) << "}" << endl;
						gd_ptr->sum_container.get_color(min, max, (value_field[l][m][0] / value_field[l][m][3]), red, green, blue);
						out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
							<< "{0.43}{0.43}{" << (m + 0.5) << "}{" 
							<< (12 - l + 0.5) << "}" << endl;
					}
				}
				else
				{
					gd_ptr->sum_container.get_color(min, max, (value_field[l][m][0] / value_field[l][m][3]), red, green, blue);
					out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
						<< "{0.73}{0.73}{" << (m + 0.5) << "}{" 
						<< (12 - l + 0.5) << "}" << endl;
					if(proc_vector.size() > 16)
					{
						gd_ptr->sum_container.get_color(min, max, value_field[l][m][1], red, green, blue);
						out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
							<< "{0.25}{0.25}{" << (m + 0.75) << "}{" 
							<< (12 - l + 0.25) << "}" << endl;
						gd_ptr->sum_container.get_color(min, max, value_field[l][m][2], red, green, blue);
						out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
							<< "{0.25}{0.25}{" << (m + 0.75) << "}{" 
							<< (12 - l + 0.75) << "}" << endl;
					}
				}
			}
		}
	}
	for(int l = 0; l < 16; l++)
	{
		if(proc_name1[l].size() > 15)
			proc_name1[l].resize(15);
		if(proc_vector.size() <= 16)
		{
			out << "  \\rput[r](-0.2," << (12 - l + 0.5) << "){" << proc_name1[l] << "}" << endl;
			out << "  \\rput[l]{90}(" << (l + 0.5) << ",13.2){" << proc_name1[l] << "}" << endl;
		}
		else
		{
			if(proc_name2[l].size() > 15)
				proc_name2[l].resize(15);
			out << "  \\rput[r](-0.2," << (12 - l + 0.8) << "){" << proc_name1[l] << "}" << endl;
			out << "  \\rput[r](-0.2," << (12 - l + 0.5) << "){-}" << endl;
			out << "  \\rput[r](-0.2," << (12 - l + 0.2) << "){" << proc_name2[l] << "}" << endl;
			out << "  \\rput[l]{90}(" << (l + 0.2) << ",13.2){" << proc_name1[l] << "}" << endl;
			out << "  \\rput[l]{90}(" << (l + 0.5) << ",13.2){-}" << endl;
			out << "  \\rput[l]{90}(" << (l + 0.8) << ",13.2){" << proc_name2[l] << "}" << endl;
			l++;
		}
	}
	out << endl; 
     /******************** color scale for P2P - value rate plot ********************/
   
	double factor = (max - min) / (double) range;
	double temp = min;
	const char* scale_unit;
	double unit;
	j = 0.0;
	out.precision(3);
	if((type == P2P_AV_DUR) || (type == P2P_SUM_DUR))
	{
		double scale_index = (max - min) / 2.0; 
		if(scale_index < 0.1)
		{
			unit = 1;
			scale_unit = SECOND;
			out.precision(6);
		}
		else if(scale_index < KILO)
		{
			unit = 1;
			scale_unit = SECOND;
		}
		else if(scale_index < MEGA)
		{
			unit = KILO;
			scale_unit = K_SECOND;
		}	
		else if(scale_index < GIGA)
		{
			unit = MEGA;
			scale_unit = M_SECOND;
		}
		else
		{
			unit = GIGA;
			scale_unit = G_SECOND;
		}
	}
	else
	{
		double scale_index = (max - min) / 2.0; 
		if(scale_index < KBYTE)
		{
			unit = _BYTE;
			if(type == P2P_AV_RAT)
				scale_unit = BYTE_SEC;
			else
				scale_unit = BYTE_TEXT;
		}
		else if(scale_index < MBYTE)
		{
			unit = KBYTE;
			if(type == P2P_AV_RAT)
				scale_unit = KBYTE_SEC;
			else
				scale_unit = KBYTE_TEXT;
		}
		else if(scale_index < GBYTE)
		{
			unit = MBYTE;
			if(type == P2P_AV_RAT)
				scale_unit = MBYTE_SEC;
			else
				scale_unit = MBYTE_TEXT;
		}
		else
		{
			unit = GBYTE;
			if(type == P2P_AV_RAT)
				scale_unit = GBYTE_SEC;
			else
				scale_unit = GBYTE_TEXT;
		}
	}
	
   for(uint32_t i = 1; i <= range; i++)
   {
   	gd_ptr->sum_container.get_color(min, max, temp, red, green, blue);
		out << "  \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
			<< "{0.5}{0.5}{" << j << "}{" 
			<< (-5) << "}" << endl;
		out << "  \\rput[r]{90}(" << j << "," << (-5.5) << "){" 
										 << (temp / unit) << scale_unit << "}" << endl;
		temp += factor;
		j += 0.75;
		if(min == max)
			break;
   }
   
   if(gd_ptr->var && (proc_vector.size() > 16))
   {
   	factor = (max_var - min_var) / (double) range;
		temp = min_var;
		j = 0.0;
   	
   	for(uint32_t i = 1; i <= range; i++)
   	{
			gd_ptr->sum_container.get_gray(min_var, max_var, temp, red, green, blue);
			out << "  \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
				<< "{0.5}{0.5}{" << j << "}{" 
				<< (-12) << "}" << endl;
			out << "  \\rput[r]{90}(" << j << "," << (-12.5) << "){" 
											 << (temp / unit) << scale_unit << "}" << endl;
			temp += factor;
			j += 0.75;
			if(min_var == max_var)
				break;
		}
   }
   if((proc_vector.size() > 16) && !gd_ptr->vis && !gd_ptr->var)
   {
  		out << endl;
   	out << "\\PstDrawNode{1.000000}{0}{0}{1.46}{1.46}{0}{-11}" << endl;
   	out << "\\PstDrawNode{0}{0}{1.000000}{1.16}{1.16}{0}{-11}" << endl;
   	out << "\\PstDrawNode{0}{1}{0}{0.86}{0.86}{0}{-11}" << endl;
		out << "\\psline{<-}(0.9,-10.4)(4,-10.4)" << endl;
		out << "\\psline{<-}(0,-11.0)(4,-11)" << endl;
		out << "\\psline{<-}(0.7,-11.6)(4,-11.6)" << endl;
		out << "\\rput[l](4.5,-10.4){Maximum}" << endl;
  		out << "\\rput[l](4.5,-11){Average}" << endl;
  		out << "\\rput[l](4.5,-11.6){Minimum}" << endl;
	}
	if((proc_vector.size() > 16) && gd_ptr->vis && !gd_ptr->var)
   {
  		out << endl;
   	out << "\\PstDrawNode{0}{1}{0}{1.46}{1.46}{0}{-11}" << endl;
   	out << "\\PstDrawNode{1.000000}{0}{0}{0.5}{0.5}{0.5}{-10.5}" << endl;
   	out << "\\PstDrawNode{0}{0}{1.000000}{0.5}{0.5}{0.5}{-11.5}" << endl;
		out << "\\psline{<-}(0.7,-10.4)(4,-10.4)" << endl;
		out << "\\psline{<-}(0,-11.0)(4,-11)" << endl;
		out << "\\psline{<-}(0.7,-11.6)(4,-11.6)" << endl;
		out << "\\rput[l](4.5,-10.4){Maximum}" << endl;
  		out << "\\rput[l](4.5,-11){Average}" << endl;
  		out << "\\rput[l](4.5,-11.6){Minimum}" << endl;
	}
	out << "\\end{pspicture}" << endl;
	out << "\\end{help}" << endl;
	out << endl;
	out << "\\newpage" << endl;
	out << endl;
	
	return 0;
}

 /**************************************** tex_p2p ****************************************/

int tex_p2p(fstream& out, global_data* gd_ptr, int tex, bool sum)
{
	string chart_name;
	double ticks;
	double value;
	uint32_t i = 1;
	uint32_t range = HUGE_TEX; //range of color-scale
	float red,green,blue;  // color values for rgb
	
	vector<uint32_t> proc_vector;
	vector<uint32_t> bin1_vector;
	vector<uint32_t> bin2_vector;
	P2P_Value p2p_value;
	
	/* 
		gd_ptr->var == false
		0 = value_sent, 1 = min_value_sent, 2 = max_value_sent, 3 = value_receive, 
	   4 = min_value_receive, 5 = max_value_receive, 6 = number of processes 
	   gd_ptr->var == true and > 16 processes 
	   0 = value_sent, 1 = value_sent², 2 = value_sent_standard degression , 3 = value_receive, 
	   4 = value_receive², 5 = value_receive_standard degression, 6 = number of processes 
	*/
	double invoc[16][7];
	double length[16][7];
	string proc_name1[16];
	string proc_name2[16];
	
	for(int j = 0; j < 16; j++)
	{
		proc_name1[j].assign(" ");
		proc_name2[j].assign(" ");
		for(int k = 0; k < 7; k++)
		{
			if(((k == 1) || (k == 4)) && !gd_ptr->var)
			{
				invoc[j][k] = (double)((uint64_t) - 1);
				length[j][k] = (double)((uint64_t) - 1);
			}
			else
			{
				invoc[j][k] = 0.0;
				length[j][k] = 0.0;
			}		
		}
	}
	
	int count = 16; // 15 is the first process group in value field and 0 is the last
	double j = 0.0;
	int help;
	
	double invocation;
	double min;
	double max = 0.0;
	double max2 = 0.0;

	gd_ptr->sum_container.get_Process_Def_Key(1, proc_vector);
	ticks = (double) gd_ptr->sum_container.get_ticks(1);
	if(ticks < 1.0)
	{	
		ticks = 1.0;
		cerr << "Error in tex_p2p. No ticks given for this trace." << endl;
	}


	/* assign processes to 16 bins */	

	vector<uint32_t>::iterator it_vector = proc_vector.begin();
	help = (int) proc_vector.size();
	while(it_vector != proc_vector.end())
	{

		/* for all processes in current bin */
		for(j = 0.0; j < ((double) help / (double) count); j++)
		{

			/* first process name in bin */
			if( 0.0 == j ) 
			{
				proc_name1[16 - count].assign(gd_ptr->sum_container.get_Process_Def(1, *it_vector));
			}

			/* summ values to bin */

			//send
			p2p_value = gd_ptr->sum_container.get_P2P(1, *it_vector, 0, 0, 0);
			value = (double) p2p_value.get_invoc();
			invoc[16 - count][0] += value;
			if(gd_ptr->var && (proc_vector.size() > 16))
			{
				invoc[16 - count][1] += value * value;
			}
			else
			{
				invoc[16 - count][1] = MINIMUM(invoc[16 - count][1], value);
				invoc[16 - count][2] = MAXIMUM(invoc[16 - count][2], value);
			}
			max = MAXIMUM(max, value);
			value = (double) p2p_value.get_length();
			length[16 - count][0] += value;
			if(gd_ptr->var && (proc_vector.size() > 16))
			{
				length[16 - count][1] += value * value;
			}
			else
			{
				length[16 - count][1] = MINIMUM(length[16 - count][1], value);
				length[16 - count][2] = MAXIMUM(length[16 - count][2], value);
			}
			max2 = MAXIMUM(max2, value);
			
			//receive
			
			p2p_value = gd_ptr->sum_container.get_P2P(1, 0, *it_vector, 0, 0);
			value = (double) p2p_value.get_invoc();
			invoc[16 - count][3] += value;
			if(gd_ptr->var && (proc_vector.size() > 16))
			{
				invoc[16 - count][4] += value * value;
			}
			else
			{
				invoc[16 - count][4] = MINIMUM(invoc[16 - count][4],value);
				invoc[16 - count][5] = MAXIMUM(invoc[16 - count][5],value);
			}
			max = MAXIMUM(max, value);
			value = (double) p2p_value.get_length();
			length[16 - count][3] += value;
			if(gd_ptr->var && (proc_vector.size() > 16))
			{
				length[16 - count][4] += value * value;
			}
			else
			{
				length[16 - count][4] = MINIMUM(length[16 - count][4], value);
				length[16 - count][5] = MAXIMUM(length[16 - count][5], value);
			}
			
			max2 = MAXIMUM(max2, value);
			
			invoc[16 - count][6] += 1.0;
			length[16 - count][6] += 1.0;
			++it_vector;
		}

		/* last process name in bin */

		--it_vector;
		proc_name2[16 - count].assign(gd_ptr->sum_container.get_Process_Def(1, *it_vector));
		++it_vector;
		
		help -= (int) j;
		if(count < 1)
			cerr << "Error in tex_p2p(). Wrong count1 value." << endl;
		--count;
	}

	out.precision(2);
	
	if(max > 0)
	{
		chart_name = "Number of P2P Invocations (sum)";
		bar_data(out, invoc, gd_ptr->var, (uint32_t) proc_vector.size());
		bar_chart((uint64_t) max, out, proc_name1, proc_name2, (uint32_t) proc_vector.size(), chart_name, false, invoc, gd_ptr->var, sum);
	}

	if(max2 > 0)
	{
		chart_name = "P2P Message Length (sum)[in Byte]";
		bar_data(out, length, gd_ptr->var, (uint32_t) proc_vector.size());
		bar_chart((uint64_t) max2, out, proc_name1, proc_name2, (uint32_t) proc_vector.size(), chart_name, true, length,gd_ptr->var, sum);
	}
	

	/*
	ACHTUNG: 4-fach-Schleife in tex_p2p_values(...) --> kann bei hoher Anzahl an CPUs sehr langsam werden !!!
	*/
	/***************************** P2P - message value plots *****************************/
	
	if(sum == false) {
	if(tex == TEX_ALLPLOT)
	{
		for(i = P2P_AV_RAT; i < P2P_ALL; i++)
		{
			tex_p2p_values(proc_vector, out, gd_ptr, ticks, range, i);
		}
	}
	else
		tex_p2p_values(proc_vector, out, gd_ptr, ticks, range, P2P_AV_RAT);

	}
	/*************************** P2P - message rate histogram ***************************/
	
	if(sum == false) {
	out.precision(6);
	gd_ptr->sum_container.get_Bin1_Def_Key(1, bin1_vector);
	gd_ptr->sum_container.get_Bin2_Def_Key(1, bin2_vector);
	vector<uint32_t>::iterator it_bin1_vector;
	vector<uint32_t>::iterator it_bin2_vector;

	map<uint32_t, map<uint32_t, double> > InvocMap;
	map<uint32_t, map<uint32_t, double> >::iterator bin1_iter;
 	map<uint32_t, double>::iterator bin2_iter;

	min = 0.0;
	max = 0.0;
	it_bin1_vector = bin1_vector.begin();

	while(it_bin1_vector != bin1_vector.end())
	{
		it_bin2_vector = bin2_vector.begin();
		while(it_bin2_vector != bin2_vector.end())
		{
			p2p_value = gd_ptr->sum_container.get_P2P(1, 0, 0, *it_bin1_vector, *it_bin2_vector);
			invocation = (double) p2p_value.get_invoc();
			InvocMap[*it_bin1_vector][*it_bin2_vector] = invocation;
			if(invocation > 0.0)
			{
				if(min == 0.0)
					min = invocation;
				else
					min = MINIMUM(invocation,min);
				max = MAXIMUM(invocation,max);
			}
			++it_bin2_vector;
		}
		++it_bin1_vector;
	}
	
	out.precision(2);
	out << "{\\Large \\bf P2P - message rate histogram}" << endl;
	if(max <= 0.0)
	{
		out << "\\bigskip" << endl;
		out << "\\bigskip" << endl;
		out << "no values" << endl;
		out << "\\newpage" << endl;
		out << endl;
		
		return 0;
	}
	else
	{
		out << "\\bigskip" << endl; 
		out << endl;
		out << "\\begin{help}" << endl;
		out << "\\psset{xunit=0.5,yunit=0.25}" << endl;
		out << "\\begin{pspicture}(25,33)" << endl;
		out << "   \\psgrid[subgriddiv=0,%" << endl;
  		out << "      griddots=5,%" << endl;
  		out << "      gridlabels=0,%" << endl;
  		out << "      yunit=2](24,16)" << endl;
  	
  		it_bin1_vector = bin1_vector.begin();
		while(it_bin1_vector != bin1_vector.end())
		{
			it_bin2_vector = bin2_vector.begin();
			while(it_bin2_vector != bin2_vector.end())
			{
				/*p2p_value = gd_ptr->sum_container.get_P2P_new(1, 0, 0, *it_bin1_vector, *it_bin2_vector);
				invocation = (double) p2p_value.get_invoc();*/
				invocation = InvocMap[*it_bin1_vector][*it_bin2_vector];
				if(invocation > 0.0)
				{
					gd_ptr->sum_container.get_color_gray(min, max, invocation, red, green, blue);
					out << "    \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
						<< "{0.48}{0.48}{" << (*it_bin1_vector - 0.5) << "}{" 
						<< ((*it_bin2_vector * 2) - 1) << "}" << endl;
				}
				++it_bin2_vector;
			}
			++it_bin1_vector;
		}
  		out << "   \\psaxes[labels = no,axesstyle = axes,subticks=2,Ox=0,Oy=0,xylogBase=2,Dx=2,Dy=4]"
  		   << "{->}(26,33)" 
  			<< endl;
  		for(i = 2; i <= 32; i += 2)
		{
			out << "\\psline{-}(-0.15," << (i - 1) << ")(0," << (i - 1) << ")" << endl;
			if(i < 10)
			{
				out << "   \\rput[r](-0.2," << (i - 1) << "){" << pow(2.0, (double) i) <<"}" << endl;
			}
			else if(i < 20)
			{
				out << "   \\rput[r](-0.2," << (i - 1) << "){" << pow(2.0, (double) (i - 10)) << "K}";
				out << endl;
			}
			else if(i < 30)
			{
				out << "   \\rput[r](-0.2," << (i - 1) << "){" << pow(2.0, (double) (i - 20)) << "M}";
				out << endl;
			}
			else
			{
				if(i == 32)
				{
					out << "   \\rput[r](-0.2," << (i - 1) << "){$>$" << pow(2.0, (double) (i - 32));
					out << "G}" << endl;
				}
				else
				{
					out << "   \\rput[r](-0.2," << (i - 1) << "){" << pow(2.0, (double) (i - 30));
					out << "G}" << endl;
				}
			}
		}
		for(i = 0; i <= 23; i++)
		{
			out << "\\psline{-}(" << ((double) i  - 0.5) <<",-0.15)(" << ((double) i  - 0.5); 
			out << ",0)" << endl;
			if(i < 10)
			{
				out << "   \\rput[r]{90}(" << ((double) i  + 0.5) << ",-0.22,){";
				out << pow(2.0, (double) i) <<"}" << endl;
			}
			else if(i < 20)
			{
				out << "   \\rput[r]{90}(" << ((double) i  + 0.5) << ",-0.22,){";
				out << pow(2.0, (double) (i - 10)) << "K}" << endl;
			}
			else 
			{
				if(i == 23)
				{
					out << "   \\rput[r]{90}(" << ((double) i  + 0.5) << ",-0.22,){$>$";
					out << pow(2.0, (double) (i - 21)) << "M}" << endl;
				}
				else
				{
					out << "   \\rput[r]{90}(" << ((double) i  + 0.5) << ",-0.22,){";
					out << pow(2.0, (double) (i - 20)) << "M}" << endl;
				}
			}
		}
   	out << "   \\rput{90}(-5,15){rate [byte/sec]}" << endl;
   	out << "   \\rput{0}(13,-8){message length [byte]}" << endl;

   	/******************* color scale for P2P - message rate histogram *******************/
		double factor = (max - min) / (double)range;
		double temp = min;
   	for(i = 1; i <= range; i++)
   	{
   		gd_ptr->sum_container.get_color_gray(min, max, temp, red, green, blue);
			out << "  \\PstDrawNode{" << red << "}{" << green << "}{" << blue << "}" 
				<< "{0.48}{0.48}{" << ((int) i - 1) << "}{" 
				<< (-13) << "}" << endl;
			out << "  \\rput[r]{90}(" << ((int) i - 1) << "," << (-14.5) << "){" 
				<< (uint64_t) temp << " \\#}" << endl;
			temp += factor;
   	}
		out << "\\end{pspicture}" << endl;
		out << "\\end{help}" << endl;
		out << endl;
		out << "\\newpage" << endl;
		out << endl;
		out.precision(6);
	}
	
	/**************************** P2P - message length histogram ****************************/
	max = 0.0;
	it_bin1_vector = bin1_vector.begin();
	out << "\\def\\barData{";
	while(it_bin1_vector != bin1_vector.end())
	{
		p2p_value = gd_ptr->sum_container.get_P2P(1, 0, 0, *it_bin1_vector, 0);
		if(p2p_value.get_invoc() > 0)
		{
			out << endl;
			invocation = log((double)p2p_value.get_invoc())/log(2.0);
			max = MAXIMUM(invocation,max);
			out << (*it_bin1_vector - 1) << " " << invocation;
		}
		++it_bin1_vector;
	}

	out << "}" << endl;
	out << endl;
	out << "{\\Large \\bf P2P - message length histogram}" << endl; 
	if(max <= 0.0)
	{
		out << "\\bigskip" << endl;
		out << "\\bigskip" << endl;
		out << "no values" << endl;
		out << "\\newpage" << endl;
		out << endl;
		
		return 0;
	}
	else
	{	
		out << "\\bigskip" << endl; 
		out << endl;
		out << "\\begin{help}" << endl;
		out << "\\psset{xunit=0.5,yunit=0.6}" << endl;
		out << "\\begin{pspicture}(25," << ((uint64_t) max) + 2 << ")" << endl;
		out << "   \\psgrid[subgriddiv=0,%" << endl;
  		out << "      griddots=5,%" << endl;
  		out << "      gridlabels=0,%" << endl;
  		out << "      yunit=1](24," << ((uint32_t) max) + 1 << ")" << endl;
  		out << "   \\psaxes[labels = no,axesstyle = axes,Ox=0,Oy=0,xylogBase=2,Dx=2,Dy=2]{->}(25," 
  			<< ((uint32_t) max) + 2 << ")" << endl;
  		out << "   \\listplot[shadow=false,linecolor=blue,plotstyle=bar,barwidth=0.2cm," << endl;
   	out << "       fillcolor=red,fillstyle=solid]{\\barData}" << endl;
   	out.precision(2);
   	for(i = 0; i <= ((uint32_t) max + 1); i += 2)
		{
			out << "\\psline{-}(-0.15," << i << ")(0," << i << ")" << endl;
			if(i < 10)
			{
				out << "   \\rput[r](-0.4," << i << "){" << pow(2.0, (double) i) <<"}" << endl;
			}
			else if(i < 20)
			{
				out << "   \\rput[r](-0.4," << i << "){" << pow(2.0, (double) i) / KILO<< "K}";
				out << endl;
			}
			else if(i < 30)
			{
				out << "   \\rput[r](-0.4," << i << "){" << pow(2.0, (double) i) / MEGA<< "M}";
				out << endl;
			}
			else
			{
				out << "   \\rput[r](-0.4," << i << "){" << pow(2.0, (double) i) / GIGA;
				out << "G}" << endl;
			}
		}
		for(i = 0; i <= 24; i += 2)
		{
			out << "\\psline{-}(" << i <<",-0.15)(" <<  i; 
			out << ",0)" << endl;
			if(i < 10)
			{
				out << "   \\rput[r]{90}(" << i << ",-0.22,){";
				out << pow(2.0, (double) i) <<"}" << endl;
			}
			else if(i < 20)
			{
				out << "   \\rput[r]{90}(" << i << ",-0.22,){";
				out << pow(2.0, (double) (i - 10)) << "K}" << endl;
			}
			else 
			{
				if(i == 23)
				{
					out << "   \\rput[r]{90}(" << i << ",-0.22,){$>$";
					out << pow(2.0, (double) (i - 20)) << "M}" << endl;
				}
				else
				{
					out << "   \\rput[r]{90}(" << i << ",-0.22,){";
					out << pow(2.0, (double) (i - 20)) << "M}" << endl;
				}
			}
		}
   	out.precision(6);
   	out << "   \\rput{90}(-5," << ((uint32_t) ((max + 1.0) / 2)) << "){invocation[\\#]}" 
   		<< endl;
   	out << "   \\rput{0}(12,-4){message length[byte]}" << endl;
		out << "\\end{pspicture}" << endl;
		out << "\\end{help}" << endl;
		out << endl;
	}

	}			//my

	return 0;
}

/*********************************** Collective Operation ***********************************/

int tex_collop(fstream& out, global_data* gd_ptr, bool sum)
{
	string chart_name;
	double ticks;
	double value;
	int coll_type[4] = {OTF_COLLECTIVE_TYPE_ONE2ALL, OTF_COLLECTIVE_TYPE_ALL2ONE, 			 
	                    OTF_COLLECTIVE_TYPE_ALL2ALL, OTF_COLLECTIVE_TYPE_BARRIER};
	
	vector<uint32_t> proc_vector;
	CollOp_Value collop_value;
	gd_ptr->sum_container.get_Process_Def_Key(1, proc_vector);
	ticks = (double) gd_ptr->sum_container.get_ticks(1);
	if(ticks < 1.0)
	{	
		ticks = 1.0;
		cerr << "Error in tex_collop. No ticks given for this trace." << endl;
	}

	/* first dim.  : 16 bins for processes
		second dim. : 0 = one2all, 1 = all2one, 2 = all2all -> not used for barrier
		third dim.  : gd_ptr == false
						  0 = value_sent, 1 = min_value_sent, 2 = max_value_sent, 
		              3 = value_receive, 4 = min_value_receive, 
		              5 = max_value_receive, 6 = number of processes
		              
		              gd_ptr == true
						  0 = value_sent, 1 = value_sent², 2 = value_sent_standard degression, 
		              3 = value_receive, 4 = value_receive², 
		              5 = value_receive_standard degression, 6 = number of processes
		              
	*/
	double invoc[16][3][7]; 
	double length[16][3][7];
	double barrier[16][7];
	double help_invoc[16][7];
	double help_length[16][7];
	
	string proc_name1[16];
	string proc_name2[16];
	
	for(int j = 0; j < 16; j++)
	{
		proc_name1[j].assign(" ");
		proc_name2[j].assign(" ");
		
		for(int l = 0; l < 3; l++)
		{
			for(int k = 0; k < 7; k++)
			{
				if(((k == 1) || (k == 4)) && !gd_ptr->var)
				{
					invoc[j][l][k] = (double)((uint64_t) - 1);
					length[j][l][k] = (double)((uint64_t) - 1);
					if(l == 0)
						barrier[j][k] = (double)((uint64_t) - 1);
				}
				else
				{
					invoc[j][l][k] = 0.0;
					length[j][l][k] = 0.0;
					if(l == 0)
						barrier[j][k] = 0.0;
				}
			}
		}
	}
	
	
	int count = 16; // 15 is the first process group in value field and 0 is the last
	double j = 0.0;
	int help;
	double time;
	
	/* 0 = one2all, 1 = all2one, 2 = all2all, 3 = barrier */
	double max1[4] = {0.0, 0.0, 0.0, 0.0};
	double max2[4] = {0.0, 0.0, 0.0, 0.0};

	ticks = (double) gd_ptr->sum_container.get_ticks(1);
	if(ticks < 1.0)
	{	
		ticks = 1.0;
		cerr << "Error in tex_p2p. No ticks given for this trace." << endl;
	}


	/* assign processes to 16 bins */	

	vector<uint32_t>::iterator it_vector = proc_vector.begin();
	help = (int) proc_vector.size();
	while(it_vector != proc_vector.end())
	{

		/* for all processes in current bin */
		for(j = 0.0; j < ((double) help / (double) count); j++)
		{

			/* first process name in bin */
			if( 0.0 == j ) 
			{
				proc_name1[16 - count].assign(gd_ptr->sum_container.get_Process_Def(1, *it_vector));
			}

			/* summ values to bin */

			for(int l = 0; l <= 2; l++)
			{
				//send
				
				collop_value = gd_ptr->sum_container.get_CollOpType(1, *it_vector, coll_type[l]);
				value = (double) collop_value.get_invoc_send();
				invoc[16 - count][l][0] += value;
				if(gd_ptr->var && (proc_vector.size() > 16))
				{
					invoc[16 - count][l][1] += value * value;
				}
				else
				{	
					invoc[16 - count][l][1] = MINIMUM(invoc[16 - count][l][1],value);
					invoc[16 - count][l][2] = MAXIMUM(invoc[16 - count][l][2],value);
				}
				max1[l] = MAXIMUM(max1[l], value);
				value = (double) collop_value.get_length_send();
				length[16 - count][l][0] += value;
				if(gd_ptr->var && (proc_vector.size() > 16))
				{
					length[16 - count][l][1] += value * value;
				}
				else
				{	
					length[16 - count][l][1] = MINIMUM(length[16 - count][l][1],value);
					length[16 - count][l][2] = MAXIMUM(length[16 - count][l][2],value);
				}
				max2[l] = MAXIMUM(max2[l], value);
				
				//receive
				
				value = (double) collop_value.get_invoc_receive();
				invoc[16 - count][l][3] += value;
				if(gd_ptr->var && (proc_vector.size() > 16))
				{
					invoc[16 - count][l][4] += value * value;
				}
				else
				{	
					invoc[16 - count][l][4] = MINIMUM(invoc[16 - count][l][4],value);
					invoc[16 - count][l][5] = MAXIMUM(invoc[16 - count][l][5],value);
				}
				max1[l] = MAXIMUM(max1[l], value);
				value = (double) collop_value.get_length_receive();
				length[16 - count][l][3] += value;
				if(gd_ptr->var && (proc_vector.size() > 16))
				{
					length[16 - count][l][4] += value * value;
				}
				else
				{	
					length[16 - count][l][4] = MINIMUM(length[16 - count][l][4],value);
					length[16 - count][l][5] = MAXIMUM(length[16 - count][l][5],value);
				}
				max2[l] = MAXIMUM(max2[l], value);	
				invoc[16 - count][l][6] += 1.0;
				length[16 - count][l][6] += 1.0;
			}
			collop_value = gd_ptr->sum_container.get_CollOpType(1, *it_vector, coll_type[3]);
			value = (double) collop_value.get_invoc_send();
			barrier[16 - count][0] += value;
			if(gd_ptr->var && (proc_vector.size() > 16))
			{
				barrier[16 - count][1] += value * value;
			}
			else
			{	
				barrier[16 - count][1] = MINIMUM(barrier[16 - count][1],value);
				barrier[16 - count][2] = MAXIMUM(barrier[16 - count][2],value);
			}
			time = (double) collop_value.get_time() / ticks;
			barrier[16 - count][3] += time;
			if(gd_ptr->var && (proc_vector.size() > 16))
			{
				barrier[16 - count][4] += time * time;
			}
			else
			{
				barrier[16 - count][4] = MINIMUM(barrier[16 - count][4], time);
				barrier[16 - count][5] = MAXIMUM(barrier[16 - count][5], time);
			}
			barrier[16 - count][6] += 1.0;
			max1[3] = MAXIMUM(max1[3], value);
			max2[3] = MAXIMUM(max2[3], time);

			++it_vector;
		}

		/* last process name in bin */

		--it_vector;
		proc_name2[16 - count].assign(gd_ptr->sum_container.get_Process_Def(1, *it_vector));
		++it_vector;
		
		help -= (int) j;
		if(count < 1)
			cerr << "Error in tex_collop(). Wrong count1 value." << endl;
		--count;
	}
	
	out.precision(2);
	for(int l = 0; l <= 2; l++)
	{
		if(max1[l] == 0.0)
		{
			switch(l)
			{
				case 0 : chart_name = "ONE2ALL(sum)";break;
				case 1 : chart_name = "ALL2ONE (sum)";break;
				case 2 : chart_name = "ALL2ALL (sum)";break;
				default: cerr << "Error in tex_collop.Wrong value in switch statement. " << endl;
			}
			continue;
		}
		for(int k = 0; k < 16; k++)
		{
			for(int m = 0; m < 7; m++)
			{
				help_invoc[k][m] = invoc[k][l][m];
				help_length[k][m] = length[k][l][m];
			}
		}
		switch(l)
		{
			case 0 : chart_name = "ONE2ALL Invocation (sum)";break;
			case 1 : chart_name = "ALL2ONE Invocation (sum)";break;
			case 2 : chart_name = "ALL2ALL Invocation (sum)";break;
			default: cerr << "Error in tex_collop.Wrong value in switch statement. " << endl;
		}
		bar_data(out, help_invoc, gd_ptr->var, (uint32_t) proc_vector.size());
		bar_chart((uint64_t) max1[l], out, proc_name1, proc_name2, (uint32_t) proc_vector.size(), chart_name, false, help_invoc,gd_ptr->var, sum);
	
		switch(l)
		{
			case 0 : chart_name = "ONE2ALL Message Length (sum)[in Byte]";break;
			case 1 : chart_name = "ALL2ONE Message Length (sum)[in Byte]";break;
			case 2 : chart_name = "ALL2ALL Message Length (sum)[in Byte]";break;
			default: cerr << "Error in tex_collop.Wrong value in switch statement. " << endl;
		}
		bar_data(out, help_length, gd_ptr->var, (uint32_t) proc_vector.size());
		bar_chart((uint64_t) max2[l], out, proc_name1, proc_name2, (uint32_t) proc_vector.size(), chart_name, true, help_length, gd_ptr->var, sum);
	}
	
	/******************************* Barrier *******************************/
	
	if(gd_ptr->var && proc_vector.size())
	{
		double max_help[2] = {max1[3],max2[3]};
		if(gd_ptr->var && (proc_vector.size() > 16))
		{
			double help_lok;
			for(int k = 0; k < 16; k++)
			{
				if((barrier[k][0] > 0) && (barrier[k][6] > 1))
				{
				
					help_lok = barrier[k][0] / barrier[k][6];
					barrier[k][2] = (barrier[k][1] -( barrier[k][6] * help_lok * help_lok)) / (barrier[k][6] - 1.0);
					barrier[k][2] = sqrt(barrier[k][2]);
					max1[3] = MAXIMUM(max1[3], max_help[0] + barrier[k][2]);
				}
				else
				{
					barrier[k][2] = 0;
				}
				if((barrier[k][3] > 0) && (barrier[k][6] > 1))
				{
					help_lok = barrier[k][3] / barrier[k][6];
					barrier[k][5] = (barrier[k][4] - (barrier[k][6] * help_lok * help_lok)) / 
					                (barrier[k][6] - 1.0);
					barrier[k][5] = sqrt(barrier[k][5]);
					max2[3] = MAXIMUM(max2[3], max_help[1] + barrier[k][5]);
				}
				else
				{
					barrier[k][5] = 0;
				}
			}
		}
	}
	/************************ Barrier Invocation ************************/
	double help_barrier;
	double test_barrier;
	float  pos;
	
	if(!gd_ptr->var && (proc_vector.size() > 16))
		pos = 0.6f;
	else
		pos = 0.5f;
	if(max1[3] == 0.0)
	{	
		return 0;
	}
	if(!gd_ptr->var && (proc_vector.size() > 16))
	{
		out << "\\def\\bardataI" << endl;
		out << "{" << endl;
		for(int k = 0; k < 16; k++)
		{
			if(barrier[k][2] > 0)
			  out << (k + 0.5) << " " << (log(barrier[k][2])/log(2.0) + 1) << endl;
		}
		out << "}" << endl;
	}
	out << "\\def\\bardataII" << endl;
	out << "{" << endl;
		
	for(int k = 0; k < 16; k++)
	{
		if(barrier[k][0] > 0)
		{
			help_barrier = barrier[k][0] / barrier[k][6];
			if(barrier[k][0] < 1)
				out << (k + pos) << " " << help_barrier << endl;
			else
			  out << (k + pos) << " " << (log(help_barrier)/log(2.0) + 1) << endl;
		}
	}
	out << "}" << endl;
	if(!gd_ptr->var && (proc_vector.size() > 16))
	{
		out << "\\def\\bardataIII" << endl;
		out << "{" << endl;
		for(int k = 0; k < 16; k++)
		{
			if(barrier[k][1] > 0)
			  out << (k + 0.8) << " " << (log(barrier[k][1])/log(2.0) + 1) << endl;
		}
		out << "}" << endl;
	}
	
	out << "{\\Large \\bf Barrier Invocation [sum]}" << endl;
	out << endl;
	out << "\\bigskip" << endl;
	out << "\\bigskip" << endl;
	out << "\\bigskip" << endl;
	out << endl;
	out << "\\begin{help}" << endl;
	out << "\\psset{xunit=1,yunit=0.5}" << endl;
	out << "\\begin{pspicture}(0,0)(16," << ((uint64_t) log(max1[3])/log(2.0) + 2) << ")" << endl;
	out << "   \\psaxes[labels=no,Oy=-1,ysubticks=2,ylogBase=2,Dy=2,ytickwidth=1pt," << endl;
	out << "            ysubtickwidth=1pt,xticksize=-1 " << ((uint64_t) log(max1[3])/log(2.0) + 2) 
      << ",yticksize=0 16,ysubticksize=1," << endl;
   out << "            yticklinestyle=dotted,ysubticklinestyle=dotted]{-}(0,0)(0,0)(16.1," 
      << ((uint64_t) log(max1[3])/log(2.0) + 2) << ")" << endl;
   count = 1;
	out << "   \\rput[r](-0.2,0){0}" << endl;
	uint64_t i;
	for(i = 1; i <= ((uint64_t) max1[3])<<1; i <<= 1)
	{
		out << "\\psline{-}(-0.15," << count << ")(0," << count << ")" << endl;
		if(i < KBYTE)
		{
			out << "   \\rput[r](-0.2," << count << "){" << i <<"}" << endl;
		}
		else if(i < MBYTE)
		{
			out << "   \\rput[r](-0.2," << count << "){" << (i / KILO) << "K}" << endl;
		}
		else if(i < GBYTE)
		{
			out << "   \\rput[r](-0.2," << count << "){" << (i / MEGA) << "M}" << endl;
		}
		else
		{
			out << "   \\rput[r](-0.2," << count << "){" << (i / GIGA) << "G}" << endl;
		}
		count += 1;
	}
	if(!gd_ptr->var && (proc_vector.size() > 16))
   {
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.8," << endl;
		out << "       fillcolor=red,fillstyle=solid]{\\bardataI}" << endl;
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.6," << endl;
		out << "       fillcolor=green,fillstyle=solid]{\\bardataII}" << endl;
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.2," << endl;
		out << "       fillcolor=blue,fillstyle=solid]{\\bardataIII}" << endl;
	}
	else
	{
		out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.8," << endl;
		out << "       fillcolor=green,fillstyle=solid]{\\bardataII}" << endl;
	}
	
	if(gd_ptr->var && (proc_vector.size() > 16))
	{
		for(int k = 0; k < 16; k++)
		{
			if(barrier[k][0] > 0)
			{
				help_barrier = barrier[k][0] / barrier[k][6];
				out << "\\psframe[fillstyle=solid,fillcolor=lightgray](" << (k + 0.4) << ",";
				if(help_barrier < barrier[k][2])
					out << "0";
				else
				{		
					if((test_barrier = help_barrier - barrier[k][2]) < 1)
						out << test_barrier;
					else
					  out << log(test_barrier)/log(2.0) + 1;
				}
				out << ")(" << (k + 0.6) << ",";
				test_barrier = help_barrier + barrier[k][2];
				if(test_barrier < 1)
					out << test_barrier;
				else
				  out << log(test_barrier)/log(2.0) + 1;
				out << ")" << endl;
			}
		}
	}
	
	for(int k = 0; k < 16; k++)
	{
		if(proc_name1[k].size() > 15)
		{
			proc_name1[k].resize(15);
		}
		if(proc_vector.size() <= 16)
		{
			out << "  \\rput[r]{90}(" << (k + 0.5) << ",-0.2){" << proc_name1[k] << "}" << endl;
		}
		else
		{
			if(proc_name2[k].size() > 15)
			{
				proc_name2[k].resize(15);
			}
			out << "  \\rput[r]{90}(" << (k + 0.2) << ",-0.2){" << proc_name1[k] << "}" << endl;
			out << "  \\rput[r]{90}(" << (k + 0.5) << ",-0.2){-}" << endl;
			out << "  \\rput[r]{90}(" << (k + 0.8) << ",-0.2){" << proc_name2[k] << "}" << endl;
			k++;
		}
	}
	if(!gd_ptr->var && (proc_vector.size() > 16))
	{
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=red,linecolor=black](0,-7){Y}" << endl;
		out << "  \\rput[l](0.2,-7.0){Maximum}" << endl;
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=green,linecolor=black](0,-7.7){Y}" << endl;
		out << "  \\rput[l](0.2,-7.7){Average}" << endl;
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=blue,linecolor=black](0,-8.4){Y}" << endl;
		out << "  \\rput[l](0.2,-8.4){Minimum}" << endl;
	}
	out << "\\end{pspicture}" << endl;
	out << "\\end{help}" << endl;
	out << "\\newpage" << endl;
	
	/************************ Barrier Duration ************************/

	if(sum) return 0;
	if(!gd_ptr->var && (proc_vector.size() > 16))
	{
		out << "\\def\\bardataI" << endl;
		out << "{" << endl;
		for(int k = 0; k < 16; k++)
		{
			if(barrier[k][5] > 0)
				out << (k + 0.5) << " " << (log10(barrier[k][5] * MEGA) + 1) << endl;
		}
		out << "}" << endl;
	}
	out << "\\def\\bardataII" << endl;
	out << "{" << endl;
	for(int k = 0; k < 16; k++)
	{
		if(barrier[k][3] > 0)
		{
			help_barrier = barrier[k][3] / barrier[k][6] * MEGA;
			if(help_barrier < 1)
				out << (k + pos) << " " << help_barrier << endl;
			else
				out << (k + pos) << " " << (log10(help_barrier) + 1) << endl;
		}
	}
	out << "}" << endl;
	if(!gd_ptr->var && (proc_vector.size() > 16))
	{
		out << "\\def\\bardataIII" << endl;
		out << "{" << endl;
		for(int k = 0; k < 16; k++)
		{
			if(barrier[k][4] > 0)
				out << (k + 0.8) << " " << (log10(barrier[k][4] * MEGA) + 1) << endl;
		}
		out << "}" << endl;
	}
	out << "{\\Large \\bf Barrier Duration (sum)[in seconds]}" << endl;
	out << endl;
	out << "\\bigskip" << endl;
	out << "\\bigskip" << endl;
	out << "\\bigskip" << endl;
	out << endl;
	out << "\\begin{help}" << endl;
	out << "\\psset{xunit=1,yunit=0.5}" << endl;
	out << "\\begin{pspicture}(0,0)(16," << ((uint64_t) log10(max2[3] * MEGA) + 3) << ")" << endl;
	out << "   \\psaxes[labels=no,Oy=-1,ysubticks=2,ylogBase=10,Dy=2,ytickwidth=1pt," << endl;
   out << "            ysubtickwidth=1pt,xticksize=-1 " 
      << ((uint64_t) log10(max2[3] * MEGA) + 3) 
      << ",yticksize=0 16,ysubticksize=1," << endl;
   out << "            yticklinestyle=dotted,ysubticklinestyle=dotted]{-}(0,0)(0,0)(16.1," 
      << ((uint64_t) log10(max2[3] * MEGA) + 3) << ")" << endl;
   count = 1;
	out << "   \\rput[r](-0.2,0){0}" << endl;
	double m;
	for(m = 0.000001; m <= (max2[3]*100); m *= 10)
	{
		out << "\\psline{-}(-0.15," << count << ")(0," << count << ")" << endl;
		if(m < 1)
		{
			switch(count)
			{
				case 1 : out.precision(6);break;
				case 2 : out.precision(5);break;
				case 3 : out.precision(4);break;
				case 4 : out.precision(3);break;
				case 5 : out.precision(2);break;
				case 6 : out.precision(1);break;
			}
			out << "   \\rput[r](-0.2," << count << "){" << m <<"}" << endl;
		}
		else if(m < KILO)
		{
			out << "   \\rput[r](-0.2," << count << "){" << (uint64_t) m <<"}" << endl;
		}
		else if(m < MEGA)
		{
			out << "   \\rput[r](-0.2," << count << "){" << (uint64_t) (m / KILO) << "K}" << endl;
		}
		else if(m < GIGA)
		{
			out << "   \\rput[r](-0.2," << count << "){" << (uint64_t) (m / MEGA) << "M}" << endl;
		}
		else
		{
			out << "   \\rput[r](-0.2," << count << "){" << (uint64_t) (m / GIGA) << "G}" << endl;
		}
		count += 1;
	}
   if(!gd_ptr->var && (proc_vector.size() > 16))
   {
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.8," << endl;
		out << "       fillcolor=red,fillstyle=solid]{\\bardataI}" << endl;
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.6," << endl;
		out << "       fillcolor=green,fillstyle=solid]{\\bardataII}" << endl;
   	out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.2," << endl;
		out << "       fillcolor=blue,fillstyle=solid]{\\bardataIII}" << endl;
	}
	else
	{
		out << "   \\listplot[shadow=false,plotstyle=bar,barwidth=0.8," << endl;
		out << "       fillcolor=green,fillstyle=solid]{\\bardataII}" << endl;
	}
	
	if(gd_ptr->var && (proc_vector.size() > 16))
	{
		for(int k = 0; k < 16; k++)
		{
			if(barrier[k][3] > 0)
			{
				help_barrier = barrier[k][3] / barrier[k][6];
				out << "\\psframe[fillstyle=solid,fillcolor=lightgray](" << (k + 0.4) << ",";
				if(help_barrier < barrier[k][5])
					out << "0";
				else
				{		
					if((test_barrier = (help_barrier - barrier[k][5]) * MEGA) < 1)
						out << test_barrier;
					else
						out << log10(test_barrier) + 1;
				}
				out << ")(" << (k + 0.6) << ",";
				test_barrier = (help_barrier + barrier[k][5]) * MEGA;
				if(test_barrier < 1)
					out << test_barrier;
				else
					out << log10(test_barrier) + 1;
				out << ")" << endl;
			}
		}
	}
	
	for(int k = 0; k < 16; k++)
	{
		if(proc_name1[k].size() > 15)
		{
			proc_name1[k].resize(15);
		}
		if(proc_vector.size() <= 16)
		{
			out << "  \\rput[r]{90}(" << (k + 0.5) << ",-0.2){" << proc_name1[k] << "}" << endl;
		}
		else
		{
			if(proc_name2[k].size() > 15)
			{
				proc_name2[k].resize(15);
			}
			out << "  \\rput[r]{90}(" << (k + 0.2) << ",-0.2){" << proc_name1[k] << "}" << endl;
			out << "  \\rput[r]{90}(" << (k + 0.5) << ",-0.2){-}" << endl;
			out << "  \\rput[r]{90}(" << (k + 0.8) << ",-0.2){" << proc_name2[k] << "}" << endl;
			k++;
		}
	}
	if(!gd_ptr->var && (proc_vector.size() > 16))
	{
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=red,linecolor=black](0,-7){Y}" << endl;
		out << "  \\rput[l](0.2,-7.0){Maximum}" << endl;
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=green,linecolor=black](0,-7.7){Y}" << endl;
		out << "  \\rput[l](0.2,-7.7){Average}" << endl;
		out << "  \\fnode[framesize=0.2 0.2,fillstyle=solid,fillcolor=blue,linecolor=black](0,-8.4){Y}" << endl;
		out << "  \\rput[l](0.2,-8.4){Minimum}" << endl;
	}
	out << "\\end{pspicture}" << endl;
	out << "\\end{help}" << endl;

	return 0;
}

/***************************************** prod_tex *****************************************/

int prod_tex(int tex, global_data* gd_ptr, vector<string> counter_names, bool sum)
{
	fstream out((gd_ptr->filename_path + "_result.tex").c_str(), ios::out | ios::trunc);
	out.setf(ios::fixed, ios::floatfield);
	out.precision(6);

	tex_header(out, gd_ptr);

	if((tex == TEX_ALL) || (tex == TEX_FUNC) || (tex == TEX_ALLPLOT))
		tex_func(out, gd_ptr, counter_names, sum);
		
	out << "\\newpage" << endl;

	if((tex == TEX_ALL) || (tex == TEX_P2P) || (tex == TEX_ALLPLOT))
		tex_p2p(out, gd_ptr, tex, sum);
	
	out << "\\newpage" << endl;

	if((tex == TEX_ALL) || (tex == TEX_COLLOP) || (tex == TEX_ALLPLOT))
			tex_collop(out, gd_ptr, sum);

	tex_foot(out);
	out.close();

	return 0;
}
