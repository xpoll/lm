package cn.blmdz.wolf.item.service;

import cn.blmdz.home.common.model.Response;

public interface ItemSnapshotReadService {
   Response findById(Long var1);

   Response findByItemIdAndItemInfoMd5(Long var1, String var2);
}
