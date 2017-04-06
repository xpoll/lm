package cn.blmdz.wolf.parana.brand.service;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.parana.brand.model.Brand;

public interface BrandWriteService {
   Response create(Brand var1);

   Response update(Brand var1);
}
