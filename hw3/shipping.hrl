-record(ship, {id, name, container_cap}).
-record(container, {id, weight}).
-record(shipping_state, 
        {
          ships = [],
          containers = [],
          ports = [],
          ship_locations = [],
          ship_inventory = maps:new(),
          port_inventory = maps:new()
         }
       ).
-record(port, {id, name, docks = [], container_cap}).
