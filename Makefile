
GFORTRAN=gfortran
FFLAGS=
PROG=deltas
VERSION=0.1
EXTRA_DIST=scripts data
SOURCES= deltas.f
OBJS=${SOURCES:.f=.o}

all: ${PROG}

${PROG}: ${OBJS}
	${GFORTRAN} ${FFLAGS} -o ${PROG} ${SOURCES}

dist:
	@mkdir ${PROG}-${VERSION}
	@cp ${SOURCES} ${PROG}-${VERSION}
	@for f in ${EXTRA_DIST}; do cp -r $$f ${PROG}-${VERSION}; done
	@cp Makefile ${PROG}-${VERSION}
	@tar cvfz ${PROG}-${VERSION}.tar.gz ${PROG}-${VERSION} 
	@rm -rf ${PROG}-${VERSION}

clean:
	@rm -f ${PROG} ${OBJS} core

.f.o:
	${GFORTRAN} ${FFLAGS} -c $<

