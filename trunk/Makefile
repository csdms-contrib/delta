
GFORTRAN=gfortran
FFLAGS=
PROG=delta
VERSION=0.1
EXTRA_DIST= data/input.dat \
            scripts/ReadMe \
            scripts/flow.gmt \
            scripts/long.gmt \
            scripts/longTrace.gmt \
            scripts/new_code \
            scripts/plan.gmt \
            scripts/trans.gmt \
            scripts/transTrace.gmt
SOURCES= delta.f
OBJS=${SOURCES:.f=.o}

all: ${PROG}

${PROG}: ${OBJS}
	${GFORTRAN} ${FFLAGS} -o ${PROG} ${SOURCES}

dist:
	@mkdir -p ${PROG}-${VERSION}
	@cp ${SOURCES} ${PROG}-${VERSION}
	@for f in ${EXTRA_DIST}; do \
		mkdir -p ${PROG}-${VERSION}/`dirname $$f` && \
		cp -pr $$f ${PROG}-${VERSION}/`dirname $$f`; \
	done
	@cp Makefile ${PROG}-${VERSION}
	@tar cvfz ${PROG}-${VERSION}.tar.gz ${PROG}-${VERSION} 
	@rm -rf ${PROG}-${VERSION}

clean:
	@rm -f ${PROG} ${OBJS} core

.f.o:
	${GFORTRAN} ${FFLAGS} -c $<

