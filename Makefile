
GFORTRAN=gfortran
#FFLAGS=-Wall
FFLAGS=
PROG=deltas
VERSION=0.1
SOURCES= PROGRAM_15to18.f
OBJS=${SOURCES:.f=.o}

all: ${PROG}

${PROG}: ${OBJS}
	${GFORTRAN} ${FFLAGS} -o ${PROG} ${SOURCES}

dist:
	@mkdir ${PROG}-${VERSION}
	@cp ${SOURCES} ${PROG}-${VERSION}
	@cp Makefile ${PROG}-${VERSION}
	@tar cvfz ${PROG}-${VERSION}.tar.gz ${PROG}-${VERSION} 
	@rm -rf ${PROG}-${VERSION}

clean:
	@rm -f ${PROG} ${OBJS} core

.f.o:
	${GFORTRAN} ${FFLAGS} -c $<

