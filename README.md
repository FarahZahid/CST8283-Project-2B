# CST8283 Project #2B - Student Tuition Report
This was my second major assignment in my "Business Programming (COBOL)" course at Algonquin College. Written in COBOL using Microsoft Visual Studio 2012 and Visual COBOL, the program builds on top of <a href="https://github.com/richard1990/CST8283-Project-2A">Project #2A</a>.

The input file (STUFIL2B) contains seven student names; the student's respective student number, program number, and courses they are enrolled in; the payment to tuition they made; and the amount of tuition they still owe. Each student's grade is determined by taking the average of their four courses and given a grade rating (e.g. "A", "B", etc.) based on the following criteria:

<table>
<tr>
<td>Grade</td>
<td>Average</td>
</tr>
<tr>
<td>A</td>
<td>85 to 100</td>
</tr>
<tr>
<td>B</td>
<td>75 to 84</td>
</tr>
<tr>
<td>C</td>
<td>65 to 74</td>
</tr>
<tr>
<td>D</td>
<td>50 to 64</td>
</tr>
<tr>
<td>F</td>
<td>less than 50</td>
</tr>
</table>

The file containing the courses (COURSES) is used to fill a table and determine what course a student is enrolled in when a record is read from the input file (STUFIL2B). There are two output files: STURPT generates report card containing all the students' respective names, courses, average, and grade. The second output file (TUITNRPT) displays the students who still have to pay tuition. The amount owed is displayed using decimal points, commas, and a floating dollar sign. The user is also asked to enter the current day, month, and last two digits of the current year which is added to the tuition report. There are also various headers for each output file.

This project really built on my knowledge of COBOL as it required multiple file input/output, tables, user input, various flags to determine end-of-file and to iterate through things like tables, and various subroutines. I actually enjoyed this assignment as I found it quite a challenge to do all this using COBOL.
