clean:
	find $(CURDIR) -name .spago -type d -depth 2 -exec rm -rf {} \;
	find $(CURDIR) -name output -type d -depth 2 -exec rm -rf {} \;
