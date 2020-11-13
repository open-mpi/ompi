'''
Usage information:
    The script should be placed in the same folder as the one containing the man pages. 
        The first line after the imports specifies which folder contains the man pages,
        the program will go through this folder and look for the file/files and convert
        them into a markdown format.
    
    The two options to run the script are:
        python3 nroff-to-md-conversion.py -f myfile.3in
            converts a single file in the subdirectory from nroff to markdown

        python3 nroff-to-md-conversion.py --all
            converts every file in the subdirectory from nroff to markdown
'''

import os # os.system(command), os.chdir(folder)
import argparse
import re

os.chdir("man3")

# determine what format of the seealso is outputted
newLinks = True

parser = argparse.ArgumentParser()
parser.add_argument('-f', dest='file', type=str, help='.3in file to convert to markdown')
parser.add_argument('--all', dest='convertAll', action="store_true", default=False, help='option to batch convert all files in subdirectory')
args = parser.parse_args()

# writes the lines list to the file
def writeLines(lines, filename):
  # print them
  # for line in lines:
  #   print(line, end="")
  # print()

  # write them
  with open(filename, "w") as fh:
    for line in lines:
      fh.write(line)

  # print("Wrote file:",filename)

# helper for adjustMarkdown
def allUpper(line):
  line = line.rstrip()

  # check if line is empty, if so, return false
  if len(line) == 0:
    return False
#Some titles have punctuations like '-' in them
  noPuncLine = ''
  for letter in line:
      if letter.isalpha():
          noPuncLine+=letter
  # nromal operation
  for letter in noPuncLine:
    if not letter.isupper():
      return False

  return True

# figure out what text to add to links in the see also section
def addLink(mpiFile):
    line = ""

    # print(mpiFile)

    mpiFile = mpiFile.rstrip()

    if " " in mpiFile:
      mpiFile = mpiFile.replace(" ", "")

    if "\\" in mpiFile:
      mpiFile = mpiFile.replace("\\", "")

    if newLinks:
        # Format: [`MPI_Bcast`(3)](./?file=MPI_Bcast.md)
        line = "[`{}(3)`](./?file={}.md)\n".format(mpiFile, mpiFile)

    else:
        # Format: [`MPI_Bcast`(3)](MPI_Bcast.html)
        line = "[`{}(3)`]({}.html)\n".format(mpiFile, mpiFile)

    return line

# helper for adjustMarkdown
def startOfCodeBlock(line):
  if 'C' in line:
    return "```c\n"
  elif 'Fortran' in line:
    return "```fortran\n"

#Add appropriate   `` around function names and parameters
def adjustWords(words):
    for index in range(len(words)):
        last_mark = ''
        #check function names
        if '_' in words[index]:
            #Move the punctuation out of ``
            if(words[index][len(words[index])-1].isalnum()==False):
                last_mark = words[index][len(words[index])-1]
                words[index]=words[index][0:len(words[index])-1]
            words[index]='`{}`'.format(words[index])
        #check parameters
        elif words[index][0]=='*' and words[index][len(words[index])-1] == '*':
            if(words[index][len(words[index])-2].isalnum()==False):
                last_mark = words[index][len(words[index])-2]
                words[index]=words[index][0:len(words[index])-2]+words[index][len(words[index])-1:]
            words[index]=words[index].replace('*','`')
        #Delete unnecassary escape signs
        elif '\\' in words[index]:
            words[index]=words[index].replace('\\','')
        words[index]+=last_mark
    line = (' ').join(words)
    return line

# adds newline inside the code block if necessary
def checkBreak(line):
  editedLine = ""
  # check beginning of c
  if "#include" in line:
    editedLine += "\n"
  # check beginning of fortran
  elif "USE MPI" in line:
    editedLine += "\n"
  # check beginning of fortran2008
  elif "USE mpi_f08" in line:
    editedLine += "\n"
  # check beginning of function in c
  elif " MPI_" in line:
    editedLine += "\n"
  # check beginning of function in both fortrans
  elif "MPI_" in line and not ':' in line:
    editedLine += "\n"


  # add line and return
  editedLine += line
  return editedLine

# reads a markdown file and calls helper function processLine to process the markdown file further
def adjustMarkdown(filename):
  workingLines = []
  newLines = []
  fixedWidthWords = []

  with open(filename, "r") as fh:
    for line in fh.readlines():
      workingLines.append(line)

  inCodeBlock = False
  addText = False
  parameterLine = False
  #check whether it is in the name section
  name = False
  #Normal text section includes all sections except for parameterLine and Syntax
  normalText = False
  seeAlso = False
  for i in range(1, len(workingLines)):
    line = ""

    #delete unnecassary blank lines
    if workingLines[i].isspace():
        continue
    # titles
    elif "====" in workingLines[i]:
      if (inCodeBlock):
        newLines.append("```\n")
        newLines.append('\n')
        inCodeBlock = False

      addText = False

      # if all caps, then heading 2
      if allUpper(workingLines[i-1]):
        if "SEE ALSO" in workingLines[i-1]:
            seeAlso = True
        #add a new line after each title
        if workingLines[i-1] != "NAME\n":
            line+='\n'
        line+= '# ' + workingLines[i-1].title()+'\n'

      #Mark that this is a normal section
        if 'Syntax' not in line and 'Parameter' not in line:
            normalText = True
        else:
            normalText = False
    # else, heading 2
      else:
        line = '## ' + workingLines[i-1].title()+'\n'

    # indented blocks
    elif "    " in workingLines[i] and not normalText:
      # start code block
      inCodeBlock = True
      if len(newLines) > 1:
        if "##" in newLines[len(newLines)-1]:
          newLines.append(startOfCodeBlock(newLines[len(newLines)-1]))
          line = workingLines[i][4:]

        else:
          # line = workingLines[i][4:]
          line = checkBreak(workingLines[i][4:])
          #When changing a new line in a code block, use six spaces instead of a tab
          if(line[0]=='\t'):
              line = '    '+line[1:]
      else:
        print("HERE")
        line = "-----------HERE----------------"

    # non-indented blocks
    # check to make sure not going out of bounds
    elif i + 2 < len(workingLines):
      # get name at beginning
      if "**" in workingLines[i]:
        # line += "`"
        for letter in workingLines[i]:
          if letter != "*":
            line += letter
        # line += "`"

      # handle ':' sections
      elif workingLines[i+2][0] == ':':
        parameterLine = True
        line += '* `' # ticks will not be added later
        # line += '* '
        line += workingLines[i].rstrip()
        line += '`'
        line += ' : '
        line += workingLines[i+2][4:]
        # add word to go through other lines and syntax highlight later
        fixedWidthWords.append(workingLines[i].rstrip())

      # text blocks below description and errors
      elif len(newLines)>2:
          #If the text is not in a paramter or syntax section, add text
        if normalText:
            addText=True

        # filter headers and blank lines
        if addText and not allUpper(workingLines[i]):
          # create see also links
          if workingLines[i][len(workingLines[i]) - 2] == '\\':
            # Format: [`MPI_Bcast`(3)](MPI_Bcast.html)
            # TODO: Make a regex find for 2 'MPI_' in the same line - if so, add 2 different lines
            print('HERE: ',re.findall('MPI_'),line)
            if len(re.findall('MPI_')) > 1:
              print("split lines")
            else:
              line = addLink(workingLines[i])

            seeAlso = True

          # normal text
          else:
            line =  workingLines[i]
            #if a normal text is under name section, also add it to newLines
      elif(normalText and workingLines[i].isupper()==False):

          line = workingLines[i]


    else:
        line =  workingLines[i]

    # #adjust words for each line
    try:
      # make sure not in a code block
      if not inCodeBlock and not parameterLine and not seeAlso:
        line = adjustWords(line.split(' '))
    except:
        #if the line only has one word, skip this line
        pass


    # make things in fixedWidthWords fixed-width font if needed
    if not inCodeBlock and not parameterLine and not seeAlso:
      # check if any of the words are in the line
      for word in fixedWidthWords:
        wordAndBuffer = ' ' + word + ' ' # adds spaces around to prevent things like `comm`unicator
        # go through the line
        if wordAndBuffer in line:
          line = line.replace(word, '`' + word + '`')

    # replace any remaining tabs with spaces
    if "\t" in line:
      # print("replacing tab")
      line = line.replace("\t", "    ")

    # remove any unwanted backslashes
    if "\\" in line:
      line = line.replace("\\", "")

    # get rid of all * characters that aren't required <- doesn't work if there are code blocks in the description
    if not inCodeBlock and not parameterLine and "*" in line:
      line = line.replace("*", "")

    if seeAlso and "MPI_" in workingLines[i]:
      # line = addLink(workingLines[i][:-2])
      if len(re.findall('MPI_', line)) > 1:
        # print('HERE: ',re.findall('(MPI_[a-zA-Z_]+)', line),line)
        toAdd = re.findall('(MPI_[a-zA-Z_]+)', line)

        for i in range(1, len(toAdd)):
          newLines.append(addLink(toAdd[i]))
        # print("split lines")

        line = addLink(toAdd[0])

      else:
        line = addLink(workingLines[i])


    # finally, add line
    if(line):
      newLines.append(line)

    # at the end of the line, reset the line tag for the next iteration
    parameterLine = False

  return newLines

def runPandoc(file):
  execLine = "pandoc {} -f man -t markdown -s -o {}".format(file, file[:-3]+"md")
  # print("Running:", execLine)
  os.system(execLine)


'''
  Calls all methods to convert from .3in to md
'''
def convert(nroffFilename):
  mdFilename = nroffFilename[:-3]+"md"

  runPandoc(nroffFilename)
  lines = adjustMarkdown(mdFilename)
  writeLines(lines, mdFilename)

def convertAll():
  for filename in os.listdir():
    if ".3in" in filename:
        try:
            convert(filename)
        except:
            print("Couldn't convert", filename)

if (args.convertAll):
    convertAll()
else:
    convert(args.file)
  
