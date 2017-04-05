package io.terminus.parana.search.item;

import io.terminus.common.model.Response;

public interface ItemSearchWriteService {
   Response index(Long var1);

   Response delete(Long var1);

   Response update(Long var1);
}
