       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBKMV01.
	   DATE-WRITTEN.  12/7/2019
	   AUTHOR.        KYLER VANDERAA.
	  ******************************************
      * THIS PROGRAM READS A FILE AND CREATES  *
      * A PAINT JOB ESTIMATION REPORT.         *
      ******************************************
       ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.

		   SELECT PAINTJOB-MASTER
		      ASSIGN TO
			 'C:\INDIANHILLS\COBOL\COBKMV01\PAINTEST.DAT'
			  ORGANIZATION IS LINE SEQUENTIAL.

		   SELECT PRTOUT
		      ASSIGN TO
			 'C:\INDIANHILLS\COBOL\COBKMV01\PJOBTEST.PRT'
		      ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
	   FILE SECTION.
	   FD  PAINTJOB-MASTER
		   LABEL RECORD IS STANDARD
		   DATA RECORD IS PAINT-REC
		   RECORD CONTAINS 23 CHARACTERS.

	   01 PAINT-REC.
	       05 I-PAINT-EST-NO       PIC X(4).
		   05 PAINT-DATE.     
		      10    PAINT-YY     PIC 9(4). 
			  10    PAINT-MM     PIC 99.
			  10    PAINT-DD     PIC 99.
		   05 PAINT-WALL-SQ-FT   PIC 9(4).
		   05 PAINT-DOOR-SQ-FT   PIC 9(3).
		   05 PAINT-PRICE-GAL    PIC 99V99.

	   FD  PRTOUT
	       LABEL RECORD IS OMITTED
		   RECORD CONTAINS 132 CHARACTERS
		   DATA RECORD IS PRTLINE
		   LINAGE IS 60 WITH FOOTING AT 56.

	   01  PRTLINE               PIC X(132).

       WORKING-STORAGE SECTION.
	   01  WORK-AREA.
		   05  C-PCTR              PIC 99            VALUE 0.
		   05  MORE-RECS           PIC XXX           VALUE 'YES'.
		   05  C-TOTAL-WALL-SQ-FT  PIC 9(4)          VALUE 0.
		   05  C-GALLONS-NEEDED    PIC 999V99        VALUE 0.
		   05  C-PAINT-EST         PIC 9(5)V99       VALUE 0.
		   05  C-LABOR-EST         PIC 9(5)V99       VALUE 0.
		   05  C-TOTAL-EST         PIC 9(6)V99       VALUE 0.
		   05  C-PJCTR             PIC 999           VALUE 0. 
		   05  C-GT-GALLONS        PIC 9(5)V99       VALUE 0.
		   05  C-GT-PAINT-EST      PIC 9(8)V99       VALUE 0.
 		   05  C-GT-LABOR-EST      PIC 9(8)V99       VALUE 0.
		   05  C-GT-TOTAL-EST      PIC 9(9)V99       VALUE 0.
	   01  CURRENT-DATE-AND-TIME.
	       05  I-DATE.
			   10  I-YY         PIC 9(4).
			   10  I-MM         PIC 99.
		       10  I-DD         PIC 99.
		
	   01  COMPANY-TITLE.
		   05  FILLER          PIC X(6)    VALUE 'DATE:'.
		   05  O-MM            PIC 99.
		   05  FILLER          PIC X       VALUE '/'.
		   05  O-DD            PIC 99.
		   05  FILLER          PIC X       VALUE '/'.
		   05  O-YY            PIC 9(4).
		   05  FILLER          PIC X(37)   VALUE SPACES.
		   05  FILLER          PIC X(23)
                               VALUE 'KYLER''S PAINT ESTIMATOR'.
           05  FILLER          PIC X(48)   VALUE SPACES.
		   05  FILLER          PIC X(6)    VALUE 'PAGE:'.
		   05  O-PCTR          PIC Z9.

	   01 HEADER-LINE.
		   05  FILLER          PIC X(8)    VALUE 'ESTIMATE'.
		   05  FILLER          PIC X(23)   VALUE SPACES.
		   05  FILLER          PIC X(4)    VALUE 'WALL'.
		   05  FILLER          PIC X(7)    VALUE SPACES.
		   05  FILLER          PIC X(4)    VALUE 'DOOR'.
           05  FILLER          PIC X(6)    VALUE SPACES.
		   05  FILLER          PIC X(5)    VALUE 'TOTAL'.
		   05  FILLER          PIC X(6)    VALUE SPACES.
		   05  FILLER          PIC X(7)    VALUE 'GALLONS'.
		   05  FILLER          PIC X(6)    VALUE SPACES.
		   05  FILLER          PIC X(6)    VALUE 'PRICE/'.
		   05  FILLER          PIC X(11)   VALUE SPACES.
		   05  FILLER          PIC X(5)    VALUE 'PAINT'.
		   05  FILLER          PIC X(12)   VALUE SPACES.
		   05  FILLER          PIC X(5)    VALUE 'LABOR'.
		   05  FILLER          PIC X(12)   VALUE SPACES.
		   05  FILLER          PIC X(5)    VALUE 'TOTAL'.
       
	   01 HEADER-LINE-2.
		   05  FILLER          PIC X(7)    VALUE ' NUMBER'.
		   05  FILLER          PIC X(5)    VALUE SPACES.
		   05  FILLER          PIC X(13)   VALUE 'ESTIMATE DATE'.
		   05  FILLER          PIC X(5)    VALUE SPACES.
		   05  FILLER          PIC X(5)    VALUE 'SQ/FT'.
           05  FILLER          PIC X(6)    VALUE SPACES.
		   05  FILLER          PIC X(5)    VALUE 'SQ/FT'.
		   05  FILLER          PIC X(6)    VALUE SPACES.
		   05  FILLER          PIC X(5)    VALUE 'SQ/FT'.
		   05  FILLER          PIC X(7)    VALUE SPACES.
		   05  FILLER          PIC X(6)    VALUE 'NEEDED'.
		   05  FILLER          PIC X(6)    VALUE SPACES.
		   05  FILLER          PIC X(6)    VALUE 'GALLON'.
		   05  FILLER          PIC X(8)    VALUE SPACES.
		   05  FILLER          PIC X(8)    VALUE 'ESTIMATE'.
		   05  FILLER          PIC X(9)    VALUE SPACES.
		   05  FILLER          PIC X(8)    VALUE 'ESTIMATE'.
		   05  FILLER          PIC X(9)    VALUE SPACES.
		   05  FILLER          PIC X(8)    VALUE 'ESTIMATE'.
	  
       01 DETAIL-LINE.
		   05  FILLER             PIC X(2)        VALUE SPACES.
		   05  O-PAINT-EST-NO     PIC X(4).
		   05  FILLER             PIC X(7)        VALUE SPACES.
		   05  O-PAINT-MM         PIC 99.
		   05  FILLER             PIC X(1)        VALUE '/'.
		   05  O-PAINT-DD         PIC 99.
		   05  FILLER             PIC X(1)        VALUE '/'.
		   05  O-PAINT-YY         PIC 9(4).
		   05  FILLER             PIC X(7)        VALUE SPACES.
		   05  O-PAINT-WALL-SQ-FT PIC 9,999.
		   05  FILLER             PIC X(7)        VALUE SPACES.
		   05  O-PAINT-DOOR-SQ-FT PIC 9(3).
		   05  FILLER             PIC X(7)        VALUE SPACES.
		   05  O-TOTAL-WALL-SQ-FT PIC 9,999.
		   05  FILLER             PIC X(7)        VALUE SPACES.
		   05  O-GALLONS-NEEDED   PIC 999.99.
		   05  FILLER             PIC X(7)        VALUE SPACES.
		   05  O-PAINT-PRICE-GAL  PIC ZZ.99.
		   05  FILLER             PIC X(6)        VALUE SPACES.
		   05  O-PAINT-EST        PIC $$$,$$$.99.
		   05  FILLER             PIC X(7)        VALUE SPACES.
		   05  O-LABOR-EST        PIC $$$$,$$$.99.
		   05  FILLER             PIC X(6)        VALUE SPACES.
		   05  O-TOTAL-EST        PIC $$$$,$$$.99.

       01 GRAND-TOTAL-LINE.
		   05  FILLER              PIC X(13)          VALUE 
                                                    'GRAND TOTALS;'.
		   05  FILLER              PIC X(21)          VALUE SPACES.
		   05  FILLER              PIC X(17)          VALUE 
                                                    'TOTAL ESTIMATES: '.
		   05  O-PJCTR             PIC ZZ9.     
		   05  FILLER              PIC X(7)           VALUE SPACES.
		   05  O-GT-GALLONS        PIC ZZ,ZZZ.99.
		   05  FILLER              PIC X(15)          VALUE SPACES.
		   05  O-GT-PAINT-EST      PIC $$,$$$,$$$.99. 
		   05  FILLER              PIC X(4)           VALUE SPACES.
 		   05  O-GT-LABOR-EST      PIC $$,$$$,$$$.99. 
		   05  FILLER              PIC X(3)           VALUE SPACES.
		   05  O-GT-TOTAL-EST      PIC $$$,$$$,$$$.99. 

		  
		   
       PROCEDURE DIVISION.
	   0000-MAIN.
		   PERFORM 1000-INIT.
		   PERFORM 2000-MAINLINE
			   UNTIL MORE-RECS = 'NO'.
		   PERFORM 3000-CLOSING.
		   STOP RUN.

	   1000-INIT.
		   OPEN INPUT PAINTJOB-MASTER.
		   OPEN OUTPUT PRTOUT.

		   MOVE FUNCTION CURRENT-DATE TO  CURRENT-DATE-AND-TIME.
		   MOVE I-YY TO O-YY.
		   MOVE I-DD TO O-DD.
		   MOVE I-MM TO O-MM.

		   PERFORM 9000-READ.
		   PERFORM 9100-HDGS.

	   2000-MAINLINE.
		   PERFORM 2100-CALCS.
		   PERFORM 2200-OUTPUT.
		   PERFORM 9000-READ.

	   2100-CALCS.
		   ADD 1 TO C-PJCTR.
		   SUBTRACT PAINT-DOOR-SQ-FT FROM PAINT-WALL-SQ-FT GIVING 
           C-TOTAL-WALL-SQ-FT.
		   DIVIDE 115 INTO C-TOTAL-WALL-SQ-FT GIVING C-GALLONS-NEEDED.
		   MULTIPLY C-GALLONS-NEEDED BY PAINT-PRICE-GAL GIVING 
           C-PAINT-EST.
		   COMPUTE C-LABOR-EST = C-GALLONS-NEEDED * 3 * 23.55.
           ADD C-LABOR-EST C-PAINT-EST GIVING C-TOTAL-EST.
		   ADD C-GALLONS-NEEDED TO C-GT-GALLONS.
		   ADD C-PAINT-EST TO C-GT-PAINT-EST.
		   ADD C-LABOR-EST TO C-GT-LABOR-EST.
		   ADD C-TOTAL-EST TO C-GT-TOTAL-EST.
		   
       2200-OUTPUT.
		   MOVE I-PAINT-EST-NO TO O-PAINT-EST-NO.
		   MOVE C-TOTAL-WALL-SQ-FT TO O-TOTAL-WALL-SQ-FT.
		   MOVE PAINT-DOOR-SQ-FT TO O-PAINT-DOOR-SQ-FT.
		   MOVE PAINT-WALL-SQ-FT TO O-PAINT-WALL-SQ-FT.
		   MOVE PAINT-YY TO O-PAINT-YY.
		   MOVE PAINT-MM TO O-PAINT-MM.
		   MOVE PAINT-DD TO O-PAINT-DD.
		   MOVE C-GALLONS-NEEDED TO O-GALLONS-NEEDED.
		   MOVE C-PAINT-EST TO O-PAINT-EST
		   MOVE PAINT-PRICE-GAL TO O-PAINT-PRICE-GAL.
		   MOVE C-LABOR-EST TO O-LABOR-EST.
		   MOVE C-TOTAL-EST TO O-TOTAL-EST.
           WRITE PRTLINE FROM DETAIL-LINE
			      AFTER ADVANCING 1 LINE
				      AT EOP
				        PERFORM 9100-HDGS.
	   3000-CLOSING.
		   MOVE C-PJCTR TO O-PJCTR.
		   MOVE C-GT-GALLONS TO O-GT-GALLONS.
		   MOVE C-GT-PAINT-EST TO O-GT-PAINT-EST.
		   MOVE C-GT-LABOR-EST TO O-GT-LABOR-EST.
		   MOVE C-GT-TOTAL-EST TO O-GT-TOTAL-EST.
		   WRITE PRTLINE FROM GRAND-TOTAL-LINE
		         AFTER ADVANCING 3 LINES.
		   CLOSE PAINTJOB-MASTER
		         PRTOUT.
	   9000-READ.
		   READ PAINTJOB-MASTER
			   AT END
			     MOVE 'NO' TO MORE-RECS.
	   9100-HDGS.
		   ADD 1 TO C-PCTR.
		   MOVE C-PCTR TO O-PCTR.
		   WRITE PRTLINE FROM COMPANY-TITLE
			   AFTER ADVANCING PAGE.
		   WRITE PRTLINE FROM HEADER-LINE
			   AFTER ADVANCING 2 LINES.
		   WRITE PRTLINE FROM HEADER-LINE-2
			   AFTER ADVANCING 1 LINE.

