package cn.blmdz.wolf.search.item;

import cn.blmdz.home.common.model.Response;

public interface ItemSearchWriteService {
   Response index(Long var1);

   Response delete(Long var1);

   Response update(Long var1);
}
