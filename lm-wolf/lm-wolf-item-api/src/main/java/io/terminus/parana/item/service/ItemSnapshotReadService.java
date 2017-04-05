package io.terminus.parana.item.service;

import io.terminus.common.model.Response;

public interface ItemSnapshotReadService {
   Response findById(Long var1);

   Response findByItemIdAndItemInfoMd5(Long var1, String var2);
}
