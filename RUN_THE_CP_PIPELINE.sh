#! /bin/bash


FILES=$PWD/Channels_Split/*.tif
for f in $FILES
do
  echo "Processing $f file..."
  # take action on each file. $f store current file name
 
  # create file list file called "file_list.txt"
  cat file_list_base.txt > file_list.txt
  echo $f >> file_list.txt

#  cat file_list.txt
#  echo "-------------------------------------"

  # execute cellprofiler command

cellprofiler -c -r -i /home/user/Documents/Marker_Distribution/ -o /home/user/Documents/Marker_Distribution/cp_output -p /home/user/Documents/Marker_Distribution/Mask_outline_measure_object_distribution_pipeline.cppipe --file-list=/home/user/Documents/Marker_Distribution/file_list.txt --plugins-directory /home/user/CellProfiler/plugins


  # move cp output to folder we want
  NAME=`echo $f | awk -F'[//.]' '{print $7}'`
  FOLDER=./cp_output/$NAME
  mkdir $FOLDER
  mv ./cp_output/*.csv $FOLDER
  mv ./cp_output/Overlay.tiff $FOLDER

done

