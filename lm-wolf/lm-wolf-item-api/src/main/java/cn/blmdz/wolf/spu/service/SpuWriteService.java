package cn.blmdz.wolf.spu.service;

import java.util.Map;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.spu.dto.FullSpu;

public interface SpuWriteService {
   Response create(FullSpu var1);

   Response update(FullSpu var1);

   Response delete(Long var1);

   Response editRichText(Long var1, String var2);

   Response extras(Long var1, Map var2);
}
