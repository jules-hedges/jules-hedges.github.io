all:
	forester build
	stack build
	stack exec site rebuild
