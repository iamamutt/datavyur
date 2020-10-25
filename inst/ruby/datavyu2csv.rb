require 'Datavyu_API.rb'
require 'rbconfig'
require 'pathname'

begin
  puts "Warning: The script will search through all subfolders"

  # create a folder chooser function
  def pickFolder
    os = case RbConfig::CONFIG['host_os']
    when /darwin/ then :mac
    when /mswin|mingw/ then :windows
    else :unix
    end
    fc = Java::javax::swing::JFileChooser.new("JRuby panel")
    fc.set_dialog_title("Select a root folder containing .opf files")
    fc.set_file_selection_mode(Java::javax::swing::JFileChooser::DIRECTORIES_ONLY)
    if os == :mac
      fc.setCurrentDirectory(java.io.File.new(File.expand_path("~/Desktop")))
    elsif os == :windows
      fc.setCurrentDirectory(java.io.File.new(File.expand_path("~/Desktop")))
    else
      fc.setCurrentDirectory(java.io.File.new("/usr/bin"))
    end
    success = fc.show_open_dialog(javax.swing.JPanel.new)
    if success == Java::javax::swing::JFileChooser::APPROVE_OPTION
      return Pathname.new(fc.get_selected_file.get_absolute_path)
    else
      nil
    end
  end

  # get path of chosen folder
  rootFolder = pickFolder()

  # change current directory to rootFolder then search for .opf files
  Dir.chdir(rootFolder)
  puts "\nTraversing through '#{rootFolder}' for .opf files..."
  opfFiles = Dir.glob("**/*.opf").sort

  # make output directory
  outputDir = "datavyu_output_" + Time.now.strftime("%m-%d-%Y_%H-%M")
  puts "\nCreating new directory '#{outputDir}'"
  Dir.mkdir(outputDir)

  puts "\n\n=================BEGIN EXTRACTION=================\n\n"

  # header information
  logFile = File.new(File.join(outputDir, "log.txt"), "wb")
  logFile.write("datavyu2csv initiated at: #{Time.now.strftime('Date: %m/%d/%Y Time: %H:%M')}")

  logFile.write("\n\nFound the following files:\n")
  opfFiles.each do |opf| logFile.write("\n#{opf}") end

  # iterate through each .opf file
  opfFiles.each do |opfFile|

    currentFilePath = File.join(rootFolder, opfFile)
    filebasename = File.basename(opfFile, File.extname(opfFile))
    logText = "*Loading file: '#{filebasename}.opf'"
    puts logText
    logFile.write("\n\n#{'=' * 80}\n#{logText}")

    begin
      # load datavyu file
      $db,$pj = load_db(currentFilePath)

      # get list of column names
      columnList = getColumnList()

      if columnList.nil?
        logText = "!! File is empty"
        puts logText
        logFile.write("\n#{'-' * 80}\n#{logText}\n#{'-' * 80}\n")

      else
        # start reading in columns one-by-one
        columnList.each do |col_str|
          col = getColumn("#{col_str}")

          logText = "..Found column: #{col_str}"
          puts logText
          logFile.write("\n#{logText}\n")

          firstCell = col.cells[0] # used below to find argument names

          if firstCell.nil?
            logText = "!! No coded cells found! Skipping column"
            puts logText
            logFile.write("\n#{'-' * 80}\n#{logText}\n#{'-' * 80}\n")

          else
            # get names of custom arguments
            args = firstCell.arglist

            logText = "...Found arguments: #{args.join(', ')}"
            puts logText
            logFile.write("\n#{logText}\n")

            # create output file
            newFileStr = "#{col_str}__#{filebasename}.csv"
            csv_out = File.new(File.join(outputDir, newFileStr), "wb")
            csv_out.write("#{['file', 'column', 'ordinal', 'onset', 'offset', args].join(',')}")

            logText = "....Writing cells to file: '#{newFileStr}'"
            puts logText
            logFile.write("\n#{logText}")

            # iterate through each of the cells from the current column
            # replaces <arg> with a blank space
            # assumes first 3 args are ord, on, off and the rest are custom
            # write cell contents to file
            col.cells.each do |cell|
              cell_codes = printCellCodes(cell)
              arg_num = 3
              cell_codes.drop(3).each do |blnk|
                if blnk.downcase == "<#{args[arg_num-3]}>" # API converts everything to lowercase
                  cell_codes[arg_num] = ""
                end
                arg_num = arg_num + 1
              end
              csv_out.write("\n#{[filebasename, col_str, cell_codes].join(',')}")
            end

            # make empty line at end of file and write log info and close csv file
            csv_out.write("\n")
            logText = " ...done with .csv file"
            puts logText
            logFile.write("\n#{logText}\n")
            csv_out.close

          end
        end
      end
    rescue # if error occurs do this

      logText = "\n!! Error with file: #{opfFile} See log.\n"
      puts logText
      logFile.write("\n#{'-' * 80}\n#{logText}\n#{'-' * 80}\n\n")

    end
  end

  logFile.write("\n\n#{'>' * 80}")
  logFile.close
  puts "\n=================END EXTRACTION================="

end
