/*
    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
/*
 * Derived from work by Simon Fortelny
 */

import java.io.PrintWriter;
import java.io.IOException;
// import java.net.URI;
import java.net.InetAddress;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.OptionBuilder;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.commons.cli.PosixParser;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.BlockLocation;
import org.apache.hadoop.fs.FileStatus;
import org.apache.hadoop.fs.FileSystem;
import org.apache.hadoop.fs.Path;

public class HDFSFileFinder {
    private final static String name = "Hadoop File Finder";
    final static String NEWLINE="\n";
    private static String fsName="hdfs://localhost:9000" ;
    private static String userAndGroup="superuser,supergroup";
    private static String fileName;
    private static PrintWriter writer;
    private static FileStatus status;
    private static BlockLocation[] bLocations;
    private static boolean verbose = false;

    public static void main(String [ ] args){
        try {
            writer = new PrintWriter(System.out);
            parseCommandLineOptions(args);
        } catch (ParseException e) {
            System.out.println("There was an exception processing the supplied options");
            printUsage(writer);
            e.printStackTrace();
            System.exit(0);
        }
        getBlockLocationsFromHdfs();
    }

    private static Options createCommandLineOptions() {
        Options options = new Options();
        Option host   = OptionBuilder.withArgName( "fs.default.name" )
                            .hasArg()
                            .withDescription(  "fs.default.name of hadoop namenode e.g. hdfs://localhost:9000" )
                            .create( "h" );
        options.addOption(host);
        Option filename   = OptionBuilder.withArgName( "filename" )
                            .hasArg()
                            .withDescription(  "The file to show node locations for" )
                            .create( "f" );
        options.addOption(filename);
        Option debug      = OptionBuilder.withArgName( "verbose" )
                            .withDescription(  "Provide debug output" )
                            .create( "v" );
        options.addOption(debug);
        return options;
    }
    
    private static void printUsage(PrintWriter writer){
        final HelpFormatter usageFormatter = new HelpFormatter();  
        usageFormatter.printUsage(writer, 80, name, createCommandLineOptions());  
    }
    
    private static void parseCommandLineOptions(String [] args) throws ParseException {
	StringBuilder sb = new StringBuilder();
        Options options = createCommandLineOptions();
        CommandLineParser parser = new PosixParser();
        CommandLine cmd=null;
        cmd = parser.parse(options, args);
        
        //parse cmd line args
        if (cmd.hasOption("h")) {
            fsName = cmd.getOptionValue("h");
        }
        if (cmd.hasOption("f")) {
            fileName = cmd.getOptionValue("f");
        }

	if (cmd.hasOption("v")) {
	    verbose = true;
	    sb.append("DEBUG: File being located: ").append(fileName).append(NEWLINE);
	    writer.print(sb.toString());
	    writer.flush();
	}
    }

    private static void getBlockLocationsFromHdfs(){
	StringBuilder sb = new StringBuilder();
        Configuration conf = new Configuration();
	boolean first = true;

        // make connection to hdfs
        try {
	    if (verbose) {
		writer.println("DEBUG: Trying to connect to "+ fsName);
	    }
            FileSystem fs = FileSystem.get(conf);
            Path file = new Path(fileName);
            FileStatus fStatus = fs.getFileStatus(file);
            status=fStatus;
            bLocations= fs.getFileBlockLocations(status, 0, status.getLen());
            //print out all block locations
            for (BlockLocation aLocation : bLocations){
		String[] names = aLocation.getHosts();
		for (String name : names) {
		    InetAddress addr = InetAddress.getByName(name);
		    String host = addr.getHostName();
		    int idx = host.indexOf('.');
		    String hostname;
		    if (0 < idx) {
			hostname = host.substring(0, host.indexOf('.'));
		    } else {
			hostname = host;
		    }
		    if (first) {
			sb.append(hostname);
			first = false;
		    } else {
			sb.append(",").append(hostname);
		    }
		}
            }
	    sb.append(NEWLINE);
        } catch (IOException e) {
            writer.println("Error getting block location data from namenode");
            e.printStackTrace();
        }
        writer.print(sb.toString());
        writer.flush();
    }
}
