      PROGRAM VYELAS3

!     THIRD VERSION 
!            * simulaneous eveporation-yield in dry season
!            * stochastic variable 'Qa' generated in the program
      
!     1. VARIABLES DECLARATION
      
      INTEGER nQr, n0
      CHARACTER arquivo1*20,arquivo2*20
      REAL Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax, dQr
	  REAL Qr0, X(100), PX(100), inutil
      COMMON /HIDROL/ Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax
	  COMMON /GAMMA/ X,PX

!     2. READING FILE NAMES ON SCREEN
      
      CALL ABERTURA
      WRITE(*,*)'Type the name of the input file:'     
      READ(*,'(a20)')arquivo1
      OPEN(20,file=arquivo1)      
      WRITE(*,*)
      WRITE(*,*)'Type the name of the output file:'
      READ(*,'(a20)')arquivo2           
      OPEN(21,file=arquivo2)
      WRITE(*,*)
      
!     3. INPUT DATA: READING AND RECORDING      
      
      CALL INDATA(arquivo1,arquivo2,nQr,n0)

!     4. COMPUTATION OF YIELD-RELIABILITY CURVE     
      
      CALL LOADXPX
	  dQr=(Qrmax-Qrmin)/nQr
	  DO i1=0,nQr 
	     Qr0=Qrmin+i1*dQr
		 CALL GARANT(Qr0,G,Qrm,Qem,Qsm,n0)
		 WRITE (21,3) Qr0,G,Qrm,Qem,Qsm
		 WRITE (*,3)  Qr0,G,Qrm,Qem,Qsm
	  ENDDO
      
!     5. CLOSURE OF THE PROGRAM
      
      WRITE(21,*)'---------------------------------------------------------'
      WRITE(21,*)
      WRITE(*,*) '---------------------------------------------------------'
      WRITE(*,*)
      WRITE(21,*)'Program concluded successfully.'
      WRITE(*,*)'Program concluded successfully.'
      
    3 FORMAT(F9.3,4X,F5.1,3X,F9.3,3X,F9.3,3X,F9.3)

      END

!     6. SUBROUTINE THAT OPENS PROGRAM

      SUBROUTINE ABERTURA
      WRITE(*,*)'*****************************************************'
      WRITE(*,*)' VOLUME-YIELD ELASTICITY                 ' 
	  WRITE(*,*)
	  WRITE(*,*)' * version 3'
      WRITE(*,*)' * simulaneous eveporation-yield in dry season'
      WRITE(*,*)' * stochastic variable (Qa) generated in the program'
      WRITE(*,*)'                                                     '
      WRITE(*,*)' RESERVOIR SEDIMENTATION AND WATER AVAILABILITY     ' 
      WRITE(*,*)'                                                     '
      WRITE(*,*)' Departamento de Engenharia Hidraulica e Ambiental  '
      WRITE(*,*)' Universidade Federal do Ceara                      '
      WRITE(*,*)' Jose Carlos de Araujo, May 2004                    '
      WRITE(*,*)'*****************************************************'
      WRITE(*,*)
      WRITE(*,*)
      END

!     7. SUBROUTINE THAT READS AND WRITES INPUT DATA

      SUBROUTINE INDATA(arquivo1,arquivo2,nQr,n0)

      INTEGER nQr, n0
      CHARACTER arquivo1*20,arquivo2*20,title*20
      REAL Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax, dQr
      COMMON /HIDROL/ Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax

      WRITE(21,*) '*****************************************************'
      WRITE(21,*) ' VOLUME-YIELD ELASTICITY                 ' 
	  WRITE(21,*)
	  WRITE(21,*)' * version 3'
      WRITE(21,*)' * simulaneous eveporation-yield in dry season'
      WRITE(21,*)' * stochastic variable (Qa) generated in the program'
      WRITE(21,*) '                                                     '
      WRITE(21,*) ' RESERVOIR SEDIMENTATION AND WATER AVAILABILITY     ' 
      WRITE(21,*) '                                                     '
      WRITE(21,*) ' Departamento de Engenharia Hidraulica e Ambiental  '
      WRITE(21,*) ' Universidade Federal do Ceara                      '
      WRITE(21,*) ' Jose Carlos de Araujo, May 2004                    '
      WRITE(21,*) '*****************************************************'
      WRITE(21,*)
      WRITE(21,*)
      WRITE(21,*) 'Input data ' 
      WRITE(21,*) '---------------------------------------------------------'
      WRITE(*,*)
      WRITE(*,*) 'Input data '
      WRITE(*,*) '---------------------------------------------------------'
      READ(20,*)title
      WRITE(21,*) 'Title: ', title
      WRITE(*,*) 'Title: ', title
      READ(20,*)Qam
      READ(20,*)Cv
      READ(20,*)alfa    
      READ(20,*)Es      
	  READ(20,*)Vmax    
	  READ(20,*)Vmin
      READ(20,*)V00
      READ(20,*)nQr   
      READ(20,*)Qrmin   
	  READ(20,*)Qrmax
	  READ(20,*) n0
      WRITE(21,*) '---------------------------------------------------------'
      WRITE(21,4) ' Input file ..................................... ',arquivo1
      WRITE(21,4) ' Output file .................................... ',arquivo2
      WRITE(21,1) ' Mean inflow discharge, hm3/a, .................. ',Qam
      WRITE(21,1) ' Coefficient of variation annual discharge ...... ',Cv
	  WRITE(21,1) ' Morphological alfa parameter ................... ',alfa
	  WRITE(21,1) ' Evaporation of the dry period, m/a, ............ ',Es
	  WRITE(21,1) ' Maximum reservoir storage capacity, hm3, ....... ',Vmax
	  WRITE(21,1) ' Minimum regular operational volume, hm3, ....... ',Vmin
	  WRITE(21,1) ' Initial volume first trial, hm3, ............... ',V00
	  WRITE(21,2) ' Number of yield discharges steps, .............. ',nQr
	  WRITE(21,1) ' Minimum yield discharge, hm3/a, ................ ',Qrmin
	  WRITE(21,1) ' Maximum yield discharge, hm3/a, ................ ',Qrmax
      WRITE(21,2) ' Number of simulations in stochastic procedure .. ',n0
	  WRITE(21,*) '---------------------------------------------------------'
      WRITE(*,*) '---------------------------------------------------------'
      WRITE(*,4) ' Input file ..................................... ',arquivo1
      WRITE(*,4) ' Output file .................................... ',arquivo2
      WRITE(*,1) ' Mean inflow discharge, hm3/a, .................. ',Qam
      WRITE(*,1) ' Coefficient of variation annual discharge ...... ',Cv
	  WRITE(*,1) ' Morphological alfa parameter ................... ',alfa
	  WRITE(*,1) ' Evaporation of the dry period, m/a, ............ ',Es
	  WRITE(*,1) ' Maximum reservoir storage capacity, hm3, ....... ',Vmax
	  WRITE(*,1) ' Minimum regular operational volume, hm3, ....... ',Vmin
	  WRITE(*,1) ' Initial volume first trial, hm3, ............... ',V00
	  WRITE(*,2) ' Number of yield discharges steps, .............. ',nQr
	  WRITE(*,1) ' Minimum yield discharge, hm3/a, ................ ',Qrmin
	  WRITE(*,1) ' Maximum yield discharge, hm3/a, ................ ',Qrmax
      WRITE(*,2) ' Number of simulations in stochastic procedure .. ',n0
	  WRITE(*,*) '---------------------------------------------------------'
      WRITE(21,*)
      WRITE(21,*) 'Output data '
	  WRITE(21,*) '---------------------------------------------------------'
      WRITE(21,*) 'Qr (hm3/a)   G(%)   Qrm(hm3/a)   Qem(hm3/a)   Qsm(hm3/a) '
      WRITE(21,*) '---------------------------------------------------------'
      WRITE(*,*)
      WRITE(*,*)  'Output data '
      WRITE(*,*)  '---------------------------------------------------------'
      WRITE(*,*)  'Qr (hm3/a)   G(%)   Qrm(hm3/a)   Qem(hm3/a)   Qsm(hm3/a) '
      WRITE(*,*)  '---------------------------------------------------------'

    1 FORMAT(A50,F8.2)
	2 FORMAT(A50,I5)
    4 FORMAT(A50,A20)      

      END

!     8. SUBROUTINE THAT COMPUTES YIELD RELIABILITY G
      
      SUBROUTINE GARANT (Qr0,G,Qrm,Qem,Qsm,n0)
      
      INTEGER i2, j2, n0
      REAL Qr0, G, Qrm, Qem, Qsm
	  REAL V0, V1, V2, V4, k, PX0
	  REAL Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin
      COMMON /HIDROL/ Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax

      V4 = V00
	  Qsm = 0
	  Qem = 0
	  Qrm = 0
      j2  = 0
	  DO i2 = 1,n0
         V0 = V4
		 CALL RANDOM_NUMBER(PX0)
         CALL CALCQA(PX0,Qa)
		 V1 = V0+Qa
		 Qs = MAX(0.0,V1-Vmax)
         Qsm = Qsm + Qs
		 V2 = V1-Qs
		 CALL CALCQEQR(V2,Qr0,Qe,Qr,V4)
		 Qem = Qem + Qe
		 Qrm = Qrm + Qr
		 IF (Qr.lt.Qr0) THEN
		     j2 = j2
			 ELSE
			 j2 = j2 + 1
		 ENDIF
	  ENDDO
      G = 100*FLOAT(j2)/n0
      Qsm = Qsm/n0
      Qem = Qem/n0
	  Qrm = Qrm/n0
      
      END

!     9. SUBROUTINE THAT COMPUTES ANNUAL INFLOW (Qa)
      
      SUBROUTINE CALCQA (PX0,Qa)
      
	  INTEGER i6, n6
	  REAL Qa, d0, di, PX0
	  REAL Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax, dQr
	  REAL X(100), PX(100)
      COMMON /HIDROL/ Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax
	  COMMON /GAMMA/ X,PX

      n6  = 100
	  di  = 1
	  IF (PX0.ge.PX(n6)) THEN
	     Qa = X(n6)
	  ELSE
	    	IF (PX0.eq.0.0) THEN
		       PX0 = 0.00001
		    ENDIF
		 i6 = 0
		 DO WHILE (i6.le.n6)
		    i6 = i6 + 1
		    d0  = di
	        di  = PX0 - PX(i6)
		    IF (d0*di.le.0.0) THEN
		       Qa = (X(i6-1)*ABS(di)+X(i6)*ABS(d0))/(ABS(di)+ABS(d0))
			   i6 = n6 + 1
		    ENDIF
	     ENDDO
	  ENDIF
	  END

!     10. SUBROUTINE THAT COMPUTES EVAPORATION (Qe) AND YIELD (Qr)
      
      SUBROUTINE CALCQEQR (V2,Qr0,Qe,Qr,V4)

      LOGICAL duvida
	  REAL V2, Qr0, Qe, Qr, V4
	  REAL V3, Vaux, h2, h3
	  REAL Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin
      COMMON /HIDROL/ Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax

      duvida = .true.
      h2  = (V2*1000000/alfa)**0.33333333
!     Case I: initial volume below critical (no success)
      IF (V2.le.Vmin) THEN
	     h3 = h2 - Es
		 IF (h3.le.0.0) THEN
		    Qr  = 0
			Qe  = V2
			V4  = 0
			duvida = .false.
		 ELSE
		    V3  = alfa*(h3**3)/1000000
			Qe  = V2 - V3
	        Qr  = MIN(Qr0/2,V3)
			V4  = V2 - Qe - Qr
			duvida = .false.
		 ENDIF
      ENDIF
!     Case II: initial volume high enough (success)
	  Vaux = Vmin + Es*3*alfa*(h2**2)/1000000 + Qr0
	  IF (V2.ge.Vaux) THEN
	     Qr  = Qr0
		 CALL CALCV4(V2,Qr,V4)
         Qe  = V2-Qr-V4
		 duvida = .false.
	  ENDIF
!     Case III: initial volume in "gray-zone" (doubt)
      IF (duvida) THEN
		 Qr = Qr0
		 CALL CALCV4(V2,Qr,V4)
      	 Qe = V2-Qr-V4
	     IF (V4.ge.Vmin) THEN
		    duvida = .false.
		 ELSE
	        Qr = Qr0/2
			CALL CALCV4(V2,Qr,V4)
        	Qe = V2-Qr-V4
!           check if V4 < 0
            IF (V4.ge.0.0) THEN
		       duvida = .false.
		    ELSE
			   Qr = Qr/2
			   CALL CALCV4(V2,Qr,V4)
			   Qe = V2-Qr-V4
               IF (V4.lt.0.0) THEN
                  V4 = 0
                  Qr = V2/2
			      Qe = V2/2
               ENDIF
		   ENDIF
        ENDIF
	  ENDIF
	  END

!     11. SUBROUTINE THAT COMPUTES V4 GIVEN V2, Es AND Qr

      SUBROUTINE CALCV4 (V2,Qr,V4)

      INTEGER i4
	  REAL V2, Qr, V4, h2, h4i, h4, err, tol, B
	  REAL Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin

      COMMON /HIDROL/ Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax

      h2  = (1000000*V2/alfa)**0.3333333
	  B   = (h2**3)-1.5*Es*(h2**2)-Qr*1000000/alfa
	  h4  = h2-Qr/(6*alfa*(h2**2))
      IF (B.le.0.0) THEN
	     B = 0.00001
!		 WRITE(*,*) 'B negative...'
	  ENDIF
	  i4  = 0
      tol = 0.001
	  err = 999
	  DO WHILE(err.gt.tol)
      i4  = i4+1
	  h4i = h4
	  h4  = (B/(h4i+1.5*Es))**0.5
	  err = ABS(h4-h4i)/h4
	  IF(i4.gt.2000) THEN
		 WRITE(*,5)  'No convergence after 2000 trials (err = ',err,')'
		 WRITE(21,5) 'No convergence after 2000 trials (err = ',err,')'
	     err = tol/2
      ENDIF
      ENDDO
	  V4 = alfa*(h4**3)/1000000

    5 FORMAT(A41,F8.5,A2)

      END

!     12. SUBROUTINE THAT COMPUTES P(X) FOR GAMMA DISTRIBUTION

      SUBROUTINE LOADXPX

      INTEGER i5, j5, n5, nn5
	  REAL dX, aux, b, a, G, IX, xk1(32), Ak2(7), xk2(7)
	  DOUBLE PRECISION Ak1(32)
      REAL Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax
  	  REAL X(100), PX(100)
      COMMON /HIDROL/ Qam, Cv, alfa, Es, Vmax, Vmin, V00, Qrmin, Qrmax
	  COMMON /GAMMA/ X,PX

!     initialization of values
      n5    = 100
      nn5   = 4
	  dX    = Qam*(1+nn5*Cv)/n5
      X(1)  = 0
	  PX(1) = 0
	  a     = (1/Cv)**2
	  b     = Qam/a

!     loading values for the quadratures
		xk1(1) = 0.044489
		xk1(2) = 0.234526
		xk1(3) = 0.576885
		xk1(4) = 1.072449
		xk1(5) = 1.722409
		xk1(6) = 2.528337
		xk1(7) = 3.492213
		xk1(8) = 4.616457
		xk1(9) = 5.903959
		xk1(10) = 7.358127
		xk1(11) = 8.982941
		xk1(12) = 10.783019
		xk1(13) = 12.763698
		xk1(14) = 14.93114
		xk1(15) = 17.292454
		xk1(16) = 19.855861
		xk1(17) = 22.630889
		xk1(18) = 25.628636
		xk1(19) = 28.862102
		xk1(20) = 32.346629
		xk1(21) = 36.100495
		xk1(22) = 40.14572
		xk1(23) = 44.509208
		xk1(24) = 49.224395
		xk1(25) = 54.333721
		xk1(26) = 59.892509
		xk1(27) = 65.975377
		xk1(28) = 72.687628
		xk1(29) = 80.187447
		xk1(30) = 88.73534
		xk1(31) = 98.829543
		xk1(32) = 111.751398

		Ak1(1) = 	0.109218
		Ak1(2) = 	0.210443
		Ak1(3) = 	0.235213
		Ak1(4) = 	0.195903
		Ak1(5) = 	0.129984
		Ak1(6) = 	7.08E-02
		Ak1(7) = 	3.18E-02
		Ak1(8) = 	1.19E-02
		Ak1(9) = 	3.74E-03
		Ak1(10) = 	9.81E-04
		Ak1(11) = 	2.15E-04
		Ak1(12) = 	3.92E-05
		Ak1(13) = 	5.93E-06
		Ak1(14) = 	7.42E-07
		Ak1(15) = 	7.60E-08
		Ak1(16) = 	6.35E-09
		Ak1(17) = 	4.28E-10
		Ak1(18) = 	2.31E-11
		Ak1(19) = 	9.80E-13
		Ak1(20) = 	3.24E-14
		Ak1(21) = 	8.17E-16
		Ak1(22) = 	1.54E-17
		Ak1(23) = 	2.12E-19
		Ak1(24) = 	2.05E-21
		Ak1(25) = 	1.35E-23
		Ak1(26) = 	5.66E-26
		Ak1(27) = 	1.42E-28
		Ak1(28) = 	1.91E-31
		Ak1(29) = 	1.19E-34
		Ak1(30) = 	2.67E-38
		Ak1(31) = 	1.34E-42
		Ak1(32) = 	4.51E-48

        xk2(1) = 	-0.949107912
		xk2(2) = 	-0.741531186
		xk2(3) = 	-0.405845151
		xk2(4) = 	0
		xk2(5) = 	0.405845151
		xk2(6) = 	0.741531186
		xk2(7) = 	0.949107912

        Ak2(1) = 	0.129484966
		Ak2(2) = 	0.279705392
		Ak2(3) = 	0.381830051
		Ak2(4) = 	0.417959184
		Ak2(5) = 	0.381830051
		Ak2(6) = 	0.279705392
		Ak2(7) = 	0.129484966


!     computation of 'n5' values of P(X) for Gamma distribution
      DO i5=2,n5
	     X(i5)  = X(i5-1) + dX
!		 computation of gamma(a) using 32 Gauss-Legendre quadrature
		 G = 0
		 DO j5=1,32
		    G = G + Ak1(j5)*(xk1(j5)**(a-1))
		 ENDDO
!        computation of integral IX using 7 Gauss-Laguerre quadrature
         IX = 0
		 DO j5=1,7
		    aux = Ak2(j5)*((X(i5)/2)**a)*((xk2(j5)+1)**(a-1))
		    IX  = IX + aux*EXP(-(xk2(j5)+1)*(X(i5)/(2*b)))
		 ENDDO
!        computation of P(X) given X, a, b
         PX(i5) = IX/(b**a)/G
	  ENDDO
	  END