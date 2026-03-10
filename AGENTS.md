
- `make build` to build bbt and tools 

- to understand bbt : docs/bbt-skill
  
- test procedure
  - to run a specific test       : `cd tests & ./bbt <test_file>`
  - to run sanity checks (rapid) : `cd tests & ./bbt ../docs/examples`  
  - to run functional tests      : `cd tests & ./bbt ../docs/features` 
  - ajoute l'option `--exclude Windows_Only` quand tu es sur Linux/MacOS
  - ajoute l'option `--exclude Unix_Only`    quand tu es sur Windows
 
- to do list : 
  - docs/proposed_features
  - docs/fixme_index.md
  - chapter TDL in docs/project.md
  
- about design and tests
  - docs/developer_guide.md

- bbt is tested mostly with bbt
