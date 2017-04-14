Feature: Using variables in sql

Scenario: Get the query to be executed at point
  When I insert:
  """
  @userId = 1234

  SELECT name FROM users WHERE id = @userId;
  """
  And I go to end of buffer
  And I build the query and insert it
  Then I should see "SELECT name FROM users WHERE id = 1234\p;"
