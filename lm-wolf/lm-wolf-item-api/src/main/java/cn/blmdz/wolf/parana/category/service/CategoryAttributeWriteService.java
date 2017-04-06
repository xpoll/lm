package cn.blmdz.wolf.parana.category.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.category.dto.ExchangeIndexDto;
import cn.blmdz.wolf.parana.category.model.CategoryAttribute;

public interface CategoryAttributeWriteService {
   Response create(CategoryAttribute var1);

   Response update(CategoryAttribute var1);

   Response delete(Long var1);

   Response exchangeIndex(ExchangeIndexDto var1);
}
