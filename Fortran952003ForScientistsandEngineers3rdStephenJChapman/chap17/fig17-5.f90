CALL calc ( a1, a2, result, *100, *999 )

! Normal return--continue execution.
100 ...
...
STOP

! Error in subroutine call--process error and stop.  
999 WRITE (*,*) 'Error in subroutine calc.  Execution aborted.'
STOP 999
END PROGRAM

SUBROUTINE calc ( a1, a2, result, *, * )
REAL a1, a2, result, temp

IF ( a1 * a2 >= 0. ) THEN 
   result = SQRT(a1 * a2)
ELSE
   RETURN 2
END IF

RETURN 1
END SUBROUTINE calc
