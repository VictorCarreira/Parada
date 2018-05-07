PROGRAM parada

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
    !Programa do teste do critério de parada da RNA                              !
    !Orientador: Cosme Ferreira da Ponte Neto                                    !
    !Aluno: Victor Ribeiro Carreira                                              !
    !Categoria: RNA                                                              !
    !Objetivo:                                                                   !
    !Calcular A, B e C através do método dos mínimos quadrados não lineares, de  !
    !            modo a ajustar os dados abaixo a uma função exponencial do tipo:!
    !            y=A.{e}^{−Bx}+C.                                                !
    !            Fazer uma rotina em FORTRAN para ser futuramente aproveitada    !
    !Para usar compilação com flags utilize:                                     !
    !gfortran -fbounds-check -fbacktrace -Wall -Wextra -pedantic                 !
    !"pasta/subpasta/nomedopragrama.f95" -o nomedoexecutável                     !
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

                          !***********TABELA DE VARIÁVEIS***********!
                          !Conv: matriz de dados de convergência da !
                          !       rede                              !      
                          !-----------------------------------------!

  IMPLICIT NONE

  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SP):: i, ie
  INTEGER(KIND=SP)::np

  REAL(KIND=DP):: inicio,final
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: Conv
  REAL(KIND=DP), PARAMETER::pi=3.141592653
  
  CALL CPU_TIME(inicio)


   OPEN(1,FILE='Dado.csv')
   OPEN(2,FILE='Saida.csv')


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!LENDO OS ARQUIVOS DE ENTRADA!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



READ(2,15) cab    ! lê cabeçalho e armazena em cab
READ(2,15) cab    ! lê linha em branco abaixo do cabeçalho e armazena em cab

 ie=1
DO WHILE (.TRUE.)
  READ(2,*,END=8) rg(ie,1)
  ie=ie+1
END DO
8 CONTINUE
CLOSE(2)

np=ie-1  ! numero de pontos da entrada
WRITE(6,*) "n de dados de entrada",np
WRITE(6,*)'coordenada dos pontos'

DO i=1,np
  WRITE(6,*) g(i,1),'    ',g(i,2)
END DO



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!LENDO OS ARQUIVOS DOS GRUPOS!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ij=1
DO WHILE (.TRUE.)
  READ(1,*,end=9) x(ij,1),y(ij,2)
  ij=ij+1
END DO
9 CONTINUE
CLOSE(1)

nt1=ij-1 !número de dados do grupo1
WRITE(6,*) "n de dados do grupo 1",nt1

ij=1
DO WHILE (.TRUE.)
  READ(4,*,END=10) tr2(ij,1),tr2(ij,2),a1
  ij=ij+1
END DO
10 CONTINUE
CLOSE(4)
 
 nt2=ij-1 !número de dados do grupo1

WRITE(6,*) "n de dados do grupo 2",nt2

Conv=0d0 

ALLOCATE(Conv1(np,2))
   

  CLOSE(1)
  CLOSE(2)

  ! Formatos dos arquivos de saida
  21 FORMAT(A9,2x,E12.2)
  22 FORMAT(A20,2x,I10)

  CALL CPU_TIME(final)
  PRINT*,'tempo de máquina=',final-inicio
  !*******************************************************************************************!
  CONTAINS

  

     SUBROUTINE INVERT(A,i)
        integer i,im,j,k,l
        real*8 A(i,i),B(i)

         IM=I-1

         DO 5 K=1,I
           DO 2 J=1,IM
             2 B(J)=A(1,J+1)/A(1,1)
             B(I)=1.d0/A(1,1)
             DO 4 L=1,IM
               DO 3 J=1,IM
                 3 A(L,J)=A(L+1,J+1)-A(L+1,1)*B(J)
                 4 A(L,I)=-A(L+1,1)*B(I)
                 DO 5 J=1,I
                   5 A(I,J)=B(J)

     END SUBROUTINE INVERT




END PROGRAM parada