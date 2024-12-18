# Feature:

Some tests require to enter value, or to press some key.

# Scenario: testing the `enter` keyword

- Given the new file `config.ini` containing `lang=fr`

- When I run `rpl --prompt fr uk config.ini`

- Then I get 
```
rpl: Replacing "fr" with "uk" (case sensitive; partial words matched)
Save "config.ini"? ([Y]/N)
```
  
- When I enter `Y`

- Then I get 
```
Saved

rpl: 1 matches replaced in 1 out of 1 file
```

- And file `config.ini` is `lang=uk`