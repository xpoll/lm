package io.terminus.parana.search.item;

import io.terminus.common.model.Response;

public interface ItemDumpService {
   Response fullDump();

   Response deltaDump(Integer var1);
}
