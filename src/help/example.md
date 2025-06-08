# Feature : grep matching may be case insensitive

## Background: 
- Given the new file `flowers.lst`
~~~
Rose
cactus
rose
Tulip
~~~

## Scenario : default case sensitive matching  
- When I run `grep rose flowers.lst`
- Then I get 
  ~~~
  rose
  ~~~

## Scenario : case insensitive matching  
- When I run `grep -i rose flowers.lst`
- Then I get 
  ~~~
  Rose
  rose
  ~~~
