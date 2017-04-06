package cn.blmdz.wolf.parana.storage.service;

import cn.blmdz.home.common.model.Response;

public interface StorageService {
   Response findBy(Long var1, Integer var2, Integer var3);

   Response decreaseBy(Long var1, Integer var2, Integer var3, Integer var4);

   Response increaseBy(Long var1, Integer var2, Integer var3, Integer var4);

   Response set(Long var1, Integer var2, Integer var3, Integer var4);
}
