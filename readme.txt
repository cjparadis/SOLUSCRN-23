SOLUSCRN-23 V1.0 ALPHA is a computer program written in Fortran 90/95 to analyze solute data from field tracer tests 
conducted in groundwater. The program computes the expected concentrations of up to 28 solutes, including groundwater
quality parameters, e.g., pH, Alk, Temp., etc., based on the measured conentration of a single non-reactive solute.  The
non-reactive solute is deemed non-reacitve, apriori, and typcially includes an added halide tracer such as bromide or iodide,
but can include natural halides such as chorlide or even non-solute tracers such as heat, i.e., temperature.  Any differences
between the measured and expected concentration of a potentially reactive solute may be attributed to one or more flow and/or
transport mechanisms that differ from the non-reactive solute. For a more complete description of this computation, and its 
assumptions, please see this publication: https://ngwa.onlinelibrary.wiley.com/doi/10.1111/gwat.12770

The folder named "SOLUSCRN" contains 6 files as follows:

1) readme.txt
2) soluscrn_alpha.exe
3) soluscrn_alpha.f95
4) input_hoss.xlsx
5) input_hoss.txt
6) output_hoss.txt

The file named 'soluscrn_alpha.exe' is an executable program the can be ran from the Command Prompt on a Windows operating system.
The program will prompt the user to type in the name of the input file, e.g., 'input_hoss.txt'. The input file must be a .txt file
and must be formatted correctly, as follows:

line 1: name of output file with file extension

line 2: number of rows of input data, this is a single integer number

line 3: number of columns of input data, this is a single integer number

line 4: row list of time and solutes measured, seperated by one or more spaces, time is first, the non-reactive solute is second,
and potentially reacitve solutes/water quality parameters follow, these are letters only and must not exceed 8 spaces, the 
total list must not exceed 29

line 5: row list of units of time and solutes measured, seperated by one or more spaces, time unit is first, the non-reactive solute
unit is second, and potentially reacitve solutes/water quality parameters units follow, these are letters only and must not exceed
8 spaces, the total list must not exceed 29

line 6: row list of the concentrations of the solutes/water quality parameters in the injection fluid, time sholud be set to zero in
this list, these are real numbers that may include a decimal point and must not exceed 8 spaces

line 7: row list of the concentrations of the solutes/water quality parameters in the aquifer fluid, time sholud be set to zero in
this list, these are real numbers that may include a decimal point and must not exceed 8 spaces

lines 8 through N: two-dimensional array of the concentrations of the solutes/water quality parameters in the extraction fluid, time
sholud be set to time units past injection in this array, these are real numbers that may include a decimal point and must not 
exceed 8 spaces

The file named 'input_hoss.xlsx' is an example how to prepare the correctly formatted input file named 'input_hoss.txt', i.e.,
simply copy and paste the values from the .xlsx into the .txt

Once the user types in the name of the input file at the Command Prompt, a check on whether the input file was opened and read
is performed and four lines of output with Yes :) indicate success, otherwise, an error occurred in the input file.

An output file .txt is automatically generated with the file name as indicated on the first line of the input file.

Finally, the file named soluscrn_alpha.f95 is the source code for the program.

The user is encouraged to run the program (soluscrn_alpha.exe) with the given input file (input_hoss.txt) in order to check that a
new output file (output_hoss.txt) is generated; the output file should be automatically overwritten and contain an updated 
time stamp.

For questions/comments/etc., please contact paradisc@uwm.edu