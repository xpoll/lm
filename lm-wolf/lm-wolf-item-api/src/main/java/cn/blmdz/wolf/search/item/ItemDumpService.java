package cn.blmdz.wolf.search.item;

import cn.blmdz.home.common.model.Response;

public interface ItemDumpService {
   Response fullDump();

   Response deltaDump(Integer var1);
}
