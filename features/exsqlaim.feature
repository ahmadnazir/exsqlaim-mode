Feature: Using variables in sql
  As a role
  I need this feature
  So that I get some value

Scenario: Getting alist of variables
  When I insert:
  """
  @userId = 1234

  SELECT name FROM users WHERE id = @userId;
  """
  And I go to end of buffer
  And I build the query and insert it
  Then I should see "SELECT name FROM users WHERE id = 1234\p;"
