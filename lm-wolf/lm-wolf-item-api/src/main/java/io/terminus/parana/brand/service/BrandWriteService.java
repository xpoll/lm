package io.terminus.parana.brand.service;

import io.terminus.common.model.Response;
import io.terminus.parana.brand.model.Brand;

public interface BrandWriteService {
   Response create(Brand var1);

   Response update(Brand var1);
}
