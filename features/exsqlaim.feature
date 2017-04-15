Feature: Using variables in sql

Scenario: Attempt to update query at point
  When I insert:
  """
  @userId = 1234

  SELECT name FROM users WHERE id = @userId;
  """
  And I go to end of buffer
  And I press "C-c C-i"
  Then I should not see "SELECT name FROM users WHERE id = 1234\p;"

Scenario: Update query at point
  When I insert:
  """
  @userId = 1234

  SELECT name FROM users WHERE id = @userId;
  """
  And I go to end of buffer
  And I turn on sql-mode
  And I press "C-c C-i"
  Then I should see "SELECT name FROM users WHERE id = 1234\p;"
