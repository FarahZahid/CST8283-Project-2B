       IDENTIFICATION DIVISION.
       PROGRAM-ID.         PROJECT2B.
       AUTHOR.             MEL SANSCHAGRIN, RICHARD BARNEY.
       DATE-WRITTEN.       MARCH 2014.
       
       ENVIRONMENT DIVISION. 
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT STUDENT-FILE
               ASSIGN TO "C:\STUFIL2B.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT STUDENT-REPORT
               ASSIGN TO "C:\STURPT.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT COURSE-FILE
               ASSIGN TO "C:\COURSES.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TUITION-REPORT
               ASSIGN TO "C:\TUITNRPT.DAT"
                   ORGANIZATION IS LINE SEQUENTIAL.
           
       DATA DIVISION.
       FILE SECTION.
      * STUDENT-FILE is an input file
       FD STUDENT-FILE.
       01 STUDENT-RECORD.
           05 STUDENT-FIRST-NAME    PIC X(20).
           05 STUDENT-LAST-NAME     PIC X(20).
           05 STUDENT-NUMBER        PIC 9(9).
           05 STUDENT-PROGRAM       PIC X(3).
           05 COURSE-1              PIC X(7).
           05 MARK-1                PIC 9(3).
           05 COURSE-2              PIC X(7).
           05 MARK-2                PIC 9(3).
           05 COURSE-3              PIC X(7).
           05 MARK-3                PIC 9(3).
           05 COURSE-4              PIC X(7).
           05 MARK-4                PIC 9(3).
           05 PAYMENT-MADE          PIC 9(4)V99.
           05 TUITION-AMOUNT        PIC 9(4)V99.
           
      * STUDENT-REPORT is an output file
       FD STUDENT-REPORT.
       01 STUDENT-REPORT-CARD       PIC X(102).
    
      * COURSE-FILE is an input file
       FD COURSE-FILE.
       01 COURSE-RECORD.
           05 COURSE-NUMBER    PIC X(7).
           05 COURSE-NAME      PIC X(9).
           
      * TUITION-REPORT is an output file
       FD TUITION-REPORT.
       01 TUITION-REPORT-CARD      PIC X(100).

       WORKING-STORAGE SECTION.
      * Local copy of the student report
       01 STUDENT-REPORT-WS.       
           05 FILLER                   PIC X(2).
           05 STUDENT-LAST-NAME-WS     PIC X(20).
           05 FILLER                   PIC X(2).
           05 STUDENT-FIRST-NAME-WS    PIC X(20).
           05 FILLER                   PIC X(2).
           05 STUDENT-COURSE-NAME-WS   PIC X(42).
           05 FILLER                   PIC X(5).
           05 STUDENT-AVERAGE-WS       PIC 9(3).
           05 FILLER                   PIC X(5).
           05 STUDENT-GRADE-WS         PIC X.
           
      * Localcopy of the tuition report     
       01 TUITION-REPORT-WS.
           05 FILLER                           PIC X(4)    VALUE    SPACES.
           05 STUDENT-LAST-NAME-TUITION-WS     PIC X(20).
           05 FILLER                           PIC X(2)    VALUE    SPACES.
           05 STUDENT-FIRST-NAME-TUITION-WS    PIC X(20).
           05 FILLER                           PIC X(2)    VALUE    SPACES.
           05 STUDENT-NUMBER-TUITION-WS        PIC 9(9).
           05 FILLER                           PIC X(2)    VALUE    SPACES.
           05 AMOUNT-OWING-WS                  PIC $$,$$9.99.
       
      * Table to hold the courses in, each course
      * has a number and name
       01 COURSE-TABLE-WS.
           05 COURSE-TABLE OCCURS 11 TIMES.
               10 COURSE-NUMBER-WS  PIC X(7).
               10 COURSE-NAME-WS    PIC X(9).
               
      * Seven days of the week 
       01 DAYS-OF-WEEK.
           05 FILLER    PIC X(9)    VALUE   "MONDAY".
           05 FILLER    PIC X(9)    VALUE   "TUESDAY".
           05 FILLER    PIC X(9)    VALUE   "WEDNESDAY".
           05 FILLER    PIC X(9)    VALUE   "THURSDAY".
           05 FILLER    PIC X(9)    VALUE   "FRIDAY".
           05 FILLER    PIC X(9)    VALUE   "SATURDAY".
           05 FILLER    PIC X(9)    VALUE   "SUNDAY".
           
      * REDEFINE DAYS-OF-WEEK into a table     
       01 DAY-TABLE REDEFINES DAYS-OF-WEEK.
           05 DAY-NAME OCCURS 7 TIMES PIC X(9).
     
      * Fields to old the tuition report date
       01 TUITION-REPORT-DATE.
           05 CUR-DAY      PIC X(9).
           05 FILLER       PIC X.
           05 CUR-MONTH    PIC 9(2).
           05 FILLER       PIC X.
           05 CUR-YEAR     PIC 9(2).
      
      * Fields to hold the courses a student
      * is enrolled in
       01 STUDENT-COURSE-LIST.
           05 COURSE-LIST-1    PIC X(9)    VALUE    SPACES.
           05 FILLER           PIC X(2)    VALUE    ", ".
           05 COURSE-LIST-2    PIC X(9)    VALUE    SPACES.
           05 FILLER           PIC X(2)    VALUE    ", ".
           05 COURSE-LIST-3    PIC X(9)    VALUE    SPACES.
           05 FILLER           PIC X(2)    VALUE    ", ".
           05 COURSE-LIST-4    PIC X(9)    VALUE    SPACES.
           
      * Counters to keep tracks of records entered
      * and written
       01 COUNTERS.
           05 FILLER            PIC X(14)   VALUE "RECORDS READ: ".
           05 RECORDS-IN-CTR    PIC 9(3).
           05 FILLER            PIC X(18)   VALUE " RECORDS WRITTEN: ".
           05 RECORDS-OUT-CTR   PIC 9(3).
       
      * Bunch of fields to keep track of various 
      * things throughout the program
       01 FLAGS-AND-CONTROLS.
           05 STUDENT-FILE-EOF-FLAG    PIC X       VALUE    "N".
           05 COURSE-FILE-EOF-FLAG     PIC X       VALUE    "N".
           05 TOTAL-MARK               PIC 9(3)    VALUE    ZERO.
      * Condition names for the student's average     
           05 STUDENT-AVERAGE PIC 9(3).
               88 A-AVERAGE VALUES 85 THROUGH 100.
               88 B-AVERAGE VALUES 75 THROUGH 84.
               88 C-AVERAGE VALUES 65 THROUGH 74.
               88 D-AVERAGE VALUES 50 THROUGH 64.
           05 STUDENT-GRADE            PIC X.
           05 TUITION-OWED             PIC 9(4)V99.
           05 SUB                      PIC 9(2).
           05 DAY-INPUT                PIC 9.
           05 MONTH-INPUT              PIC 9(2).
           05 YEAR-INPUT               PIC 9(2).
           
      * Header to be displayed at top of the
      * student report file
       01 STURPT-COLUMN-HEADER.
           05 FILLER               PIC X(2)    VALUE SPACES.
           05 STUDENT-FULL-NAME    PIC X(12)   VALUE "STUDENT NAME".
           05 FILLER               PIC X(32)   VALUE SPACES.
           05 PROGRAM-COURSES      PIC X(7)    VALUE "COURSES".
           05 FILLER               PIC X(36)   VALUE SPACES.
           05 AVG                  PIC X(7)    VALUE "AVERAGE".
           05 FILLER               PIC X       VALUE SPACES.
           05 GRADE                PIC X(5)    VALUE "GRADE".
           
      * Title to be displayed at top of the
      * tuition report file
       01 TUITNRPT-TITLE.
           05 FILLER           PIC X(20)    VALUE   SPACES.
           05 STUDENT-OWING    PIC X(22)    VALUE   "STUDENTS OWING TUITION".
           05 FILLER           PIC X(10)    VALUE   SPACES.
           05 CUR-DATE         PIC X(15)    VALUE   SPACES.
          
      * Header to be displayed at the top of the
      * tuition report file below the title
       01 TUITNRPT-COLUMN-HEADER.
           05 FILLER           PIC X(4)    VALUE    SPACES.
           05 STUDENT-LAST     PIC X(17)   VALUE    "STUDENT LAST NAME".
           05 FILLER           PIC X(5)    VALUE    SPACES.
           05 STUDENT-FIRST    PIC X(18)   VALUE    "STUDENT FIRST NAME".
           05 FILLER           PIC X(5)    VALUE    SPACES.
           05 STUDENT-NUM      PIC X(14)   VALUE    "STUDENT NUMBER".
           05 FILLER           PIC X(5)    VALUE    SPACES.
           05 AMOUNT-OWE       PIC X(12)   VALUE    "AMOUNT OWING".
          
       PROCEDURE DIVISION.
      * Mainline routine
       100-CREATE-STUDENT-AND-TUITION-REPORT.
           PERFORM 200-INIT-COURSE-TABLE.
           PERFORM 200-INIT-CREATE-STUDENT-AND-TUITION-REPORT.
           PERFORM 200-CREATE-STUDENT-AND-TUITION-RECORD
               UNTIL STUDENT-FILE-EOF-FLAG = "Y".
           PERFORM 200-TERM-CREATE-STUDENT-AND-TUITION-REPORT.
           STOP RUN.
      
      * Open the course file, initialize the table,
      * fill the table with data, then close the 
      * course file
       200-INIT-COURSE-TABLE.
           PERFORM 700-OPEN-COURSE-FILE.
           PERFORM 700-INIT-COURSE-TABLE.
           PERFORM 700-FILL-COURSE-TABLE
               VARYING SUB FROM 1 BY 1
               UNTIL SUB > 11
               OR COURSE-FILE-EOF-FLAG = "Y".
           PERFORM 700-CLOSE-COURSE-FILE.

      * Create the student and tuition report.
      * Open the files, read the records, get
      * date input, determine the date based on
      * input, initialize the counters, and write
      * the headings
       200-INIT-CREATE-STUDENT-AND-TUITION-REPORT.
           PERFORM 700-OPEN-STUDENT-FILES.
           PERFORM 700-GET-DATE-INPUT.
           PERFORM 700-DETERMINE-DATE
           PERFORM 700-INIT-READ-WRITE-CTRS.
           PERFORM 700-WRITE-HEADINGS.
           PERFORM 700-READ-STUDENT-RECORD.

      * Create the student and tuition records.
      * Calculate the student average, determine
      * the student's grade, determine what course
      * names the student belongs to, and determine
      * the amount of tuition the student owes.
      * Then write the records and check for another
      * record.
       200-CREATE-STUDENT-AND-TUITION-RECORD.
           PERFORM 700-CALCULATE-AVERAGE.
           PERFORM 700-DETERMINE-GRADE.
           PERFORM 700-DETERMINE-COURSE-NAMES
               VARYING SUB FROM 1 BY 1
               UNTIL SUB > 11.
           PERFORM 700-DETERMINE-AMOUNT-OWING.
           PERFORM 700-WRITE-STUDENT-RECORD.
           IF TUITION-OWED > 0
               PERFORM 700-WRITE-TUITION-RECORD.
           PERFORM 700-READ-STUDENT-RECORD.

      * Write the audit counters and then close the
      * files.
       200-TERM-CREATE-STUDENT-AND-TUITION-REPORT.
           PERFORM 700-WRITE-AUDIT-COUNTERS.
           PERFORM 700-CLOSE-STUDENT-FILES.

      * Open the course file for input
       700-OPEN-COURSE-FILE.
           OPEN INPUT COURSE-FILE.
      
      * Initialize the table     
       700-INIT-COURSE-TABLE.
           INITIALIZE COURSE-TABLE.
      
      * Fill the course table with data from file
      * and set EOF flag to true when at end
       700-FILL-COURSE-TABLE.
           READ COURSE-FILE AT END MOVE "Y" TO COURSE-FILE-EOF-FLAG
               NOT AT END MOVE COURSE-RECORD TO COURSE-TABLE(SUB).
               
      * Close the course file       
       700-CLOSE-COURSE-FILE.
           CLOSE COURSE-FILE. 

      * Open the files for input / output.
       700-OPEN-STUDENT-FILES.
           OPEN INPUT  STUDENT-FILE.
           OPEN OUTPUT STUDENT-REPORT.
           OPEN OUTPUT TUITION-REPORT.
      
      * Prompt user to enter date which will be
      * used in the title in the tuition report
       700-GET-DATE-INPUT.
           DISPLAY "ENTER DAY AS NUM: " COLUMN 6 LINE 6.
           ACCEPT DAY-INPUT.
           DISPLAY "ENTER MONTH AS NUM: " COLUMN 6 LINE 7.
           ACCEPT MONTH-INPUT.
           DISPLAY "ENTER YEAR (LAST 2 DIGITS): " COLUMN 6 LINE 8.
           ACCEPT YEAR-INPUT.
           
      * Determine the date by moving what the user 
      * entered as input into the date group item
       700-DETERMINE-DATE.
           MOVE DAY-NAME(DAY-INPUT) TO CUR-DAY.
           MOVE MONTH-INPUT TO CUR-MONTH.
           MOVE YEAR-INPUT TO CUR-YEAR.
           MOVE TUITION-REPORT-DATE TO CUR-DATE.     

      * Initialize the counters.     
       700-INIT-READ-WRITE-CTRS.
           INITIALIZE  RECORDS-IN-CTR
                       RECORDS-OUT-CTR.

      * Write the headings and title for the files                 
       700-WRITE-HEADINGS.
           WRITE STUDENT-REPORT-CARD  FROM STURPT-COLUMN-HEADER.
           WRITE TUITION-REPORT-CARD FROM TUITNRPT-TITLE.
           WRITE TUITION-REPORT-CARD FROM TUITNRPT-COLUMN-HEADER.
           
      * Keep reading records until the EOF flag is true
      * and keep track of records entered
       700-READ-STUDENT-RECORD.
           READ  STUDENT-FILE
               AT END  MOVE "Y" TO STUDENT-FILE-EOF-FLAG
                   NOT AT END
                   ADD  1  TO RECORDS-IN-CTR.
                   
      * Add up the four marks then divide by four to
      * get the student's average
       700-CALCULATE-AVERAGE.
           ADD MARK-1 MARK-2 MARK-3 MARK-4 GIVING TOTAL-MARK.
           DIVIDE TOTAL-MARK BY 4
               GIVING STUDENT-AVERAGE ROUNDED.

      * Determine the student's grade by using condition
      * statements
       700-DETERMINE-GRADE.
           EVALUATE TRUE
               WHEN A-AVERAGE MOVE "A" TO STUDENT-GRADE
               WHEN B-AVERAGE MOVE "B" TO STUDENT-GRADE
               WHEN C-AVERAGE MOVE "C" TO STUDENT-GRADE
               WHEN D-AVERAGE MOVE "D" TO STUDENT-GRADE
               WHEN OTHER MOVE "F" TO STUDENT-GRADE
           END-EVALUATE.

      * Determine what course names a student belongs
      * to by searching through the course name table
       700-DETERMINE-COURSE-NAMES.
           IF COURSE-1 = COURSE-NUMBER-WS(SUB)
               MOVE COURSE-NAME-WS(SUB) TO COURSE-LIST-1.
           IF COURSE-2 = COURSE-NUMBER-WS(SUB)
               MOVE COURSE-NAME-WS(SUB) TO COURSE-LIST-2.
           IF COURSE-3 = COURSE-NUMBER-WS(SUB)
               MOVE COURSE-NAME-WS(SUB) TO COURSE-LIST-3.
           IF COURSE-4 = COURSE-NUMBER-WS(SUB)
               MOVE COURSE-NAME-WS(SUB) TO COURSE-LIST-4.

      * Determine how much tuition a student
      * still owes
       700-DETERMINE-AMOUNT-OWING.
           SUBTRACT PAYMENT-MADE FROM TUITION-AMOUNT
               GIVING TUITION-OWED.
           
      * Write the student record and increment
      * the records out counter
       700-WRITE-STUDENT-RECORD.
           MOVE  STUDENT-LAST-NAME  TO  STUDENT-LAST-NAME-WS.
           MOVE  STUDENT-FIRST-NAME TO STUDENT-FIRST-NAME-WS.
           MOVE STUDENT-COURSE-LIST  TO STUDENT-COURSE-NAME-WS.
           MOVE STUDENT-AVERAGE TO STUDENT-AVERAGE-WS.
           MOVE STUDENT-GRADE TO STUDENT-GRADE-WS.
           WRITE STUDENT-REPORT-CARD  FROM  STUDENT-REPORT-WS.
           ADD  1  TO RECORDS-OUT-CTR.
      
      * Write the tuition record 
       700-WRITE-TUITION-RECORD.
           MOVE STUDENT-LAST-NAME TO STUDENT-LAST-NAME-TUITION-WS.
           MOVE STUDENT-FIRST-NAME TO STUDENT-FIRST-NAME-TUITION-WS.
           MOVE STUDENT-NUMBER TO STUDENT-NUMBER-TUITION-WS.
           MOVE TUITION-OWED TO AMOUNT-OWING-WS.
           WRITE TUITION-REPORT-CARD FROM TUITION-REPORT-WS.
           
      * Write the audit trails
       700-WRITE-AUDIT-COUNTERS.
            WRITE STUDENT-REPORT-CARD  FROM COUNTERS.
            WRITE TUITION-REPORT-CARD FROM COUNTERS.

      * Close all the files
       700-CLOSE-STUDENT-FILES.
           CLOSE STUDENT-FILE
                 STUDENT-REPORT
                 TUITION-REPORT.