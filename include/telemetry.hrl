-define(update_counter(Name), telemetry:update(Name, counter)).
-define(update_counter(Name, Value), telemetry:update(Name, counter, Value)).
-define(update_histogram(Name, Value), telemetry:update(Name, histogram, Value)).