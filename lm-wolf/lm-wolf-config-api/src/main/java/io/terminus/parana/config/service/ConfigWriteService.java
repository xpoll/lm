package io.terminus.parana.config.service;

import io.terminus.common.model.Response;
import io.terminus.parana.config.model.Config;
import java.util.List;

public interface ConfigWriteService {
   Response create(Config var1);

   Response bulkCreate(List var1);

   Response update(Config var1);

   Response bulkUpdate(List var1);

   Response delete(Long var1);
}
