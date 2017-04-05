package io.terminus.parana.spu.service;

import io.terminus.common.model.Response;
import io.terminus.parana.spu.dto.FullSpu;
import java.util.Map;

public interface SpuWriteService {
   Response create(FullSpu var1);

   Response update(FullSpu var1);

   Response delete(Long var1);

   Response editRichText(Long var1, String var2);

   Response extras(Long var1, Map var2);
}
