Feature: Resources
  In order to control outbound calls
  As a developer
  I want to manage resources

  Scenario: Get resources
    When I try to invoke /resources
    Then I should receive an HTTP Status of 200
    And I should receive valid JSON
    And I should receive a body with a segment of {"name":"Sales", "totalResources":1, "availableResources":1}
    And I should receive a body with a segment of {"name":"Service", "totalResources":15, "availableResources":15}

  Scenario: Get resources for a single segment
    When I try to invoke /resources?segment=Sales
    Then I should receive an HTTP Status of 200
    And I should receive valid JSON
    And I should receive a body with a segment of {"name":"Sales", "totalResources":1, "availableResources":1}
    And I should not receive a body with a segment of {"name":"Service", "totalResources":15, "availableResources":15}

  @cleanup
  Scenario Outline: Checkout a resource
    When I put <content_type> data <data> to /resources/checkout
    Then I should receive an HTTP Status of 200
    Then I should receive valid JSON
    And I should receive a body with a segment of {"name":"Service", "totalResources":15, "availableResources":14}
  Examples:
    | content_type                      | data                                       |
    | application/json                  | {"segment":"Service", "id":"conversation"} |
    | application/x-www-form-urlencoded | segment=Service&id=conversation            |


  Scenario Outline: Checkin a resource
    Given I put <content_type> data <data> to /resources/checkout
    When I put <content_type> data <data> to /resources/checkin
    Then I should receive an HTTP Status of 200
    Then I should receive valid JSON
    And I should receive a body with a segment of {"name":"Service", "totalResources":15, "availableResources":15}
  Examples:
    | content_type                      | data                                       |
    | application/json                  | {"segment":"Service", "id":"conversation"} |
    | application/x-www-form-urlencoded | segment=Service&id=conversation            |

  Scenario Outline: Cannot checkout without an id
    When I put <content_type> data <data> to /resources/checkout
    Then I should receive an error of no_resource
  Examples:
    | content_type                      | data                  |
    | application/json                  | {"segment":"Service"} |
    | application/x-www-form-urlencoded | segment=Service       |

  Scenario Outline: Cannot checkout without a segment
    When I put <content_type> data <data> to /resources/checkout
    Then I should receive an error of no_resource
  Examples:
    | content_type                      | data                  |
    | application/json                  | {"id":"conversation"} |
    | application/x-www-form-urlencoded | id=conversation       |

  Scenario Outline: Cannot checkin without first checking out
    When I put <content_type> data <data> to /resources/checkin
    Then I should receive an error of no_resource
  Examples:
    | content_type                      | data                                       |
    | application/json                  | {"segment":"Service", "id":"conversation"} |
    | application/x-www-form-urlencoded | segment=Service&id=conversation            |

  @cleanup
  Scenario Outline: Cannot checkout when no more resources are available
    Given I put <content_type> data <data> to /resources/checkout
    And I should receive valid JSON
    And I should receive a body with a segment of {"name":"Sales", "totalResources":1, "availableResources":0}
    When I put <content_type> data <data> to /resources/checkout
    Then I should receive an error of no_resource
  Examples:
    | content_type                      | data                                     |
    | application/json                  | {"segment":"Sales", "id":"conversation"} |
    | application/x-www-form-urlencoded | segment=Sales&id=conversation            |
