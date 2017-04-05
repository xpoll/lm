package io.terminus.parana.category.service;

import io.terminus.common.model.Response;
import io.terminus.parana.category.dto.ExchangeIndexDto;
import io.terminus.parana.category.model.CategoryAttribute;

public interface CategoryAttributeWriteService {
   Response create(CategoryAttribute var1);

   Response update(CategoryAttribute var1);

   Response delete(Long var1);

   Response exchangeIndex(ExchangeIndexDto var1);
}
