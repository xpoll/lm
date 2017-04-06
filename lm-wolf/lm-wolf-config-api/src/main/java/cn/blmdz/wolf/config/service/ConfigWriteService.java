package cn.blmdz.wolf.config.service;

import java.util.List;

import cn.blmdz.home.common.model.Response;
import cn.blmdz.wolf.config.model.Config;

public interface ConfigWriteService {
   Response create(Config var1);

   Response bulkCreate(List var1);

   Response update(Config var1);

   Response bulkUpdate(List var1);

   Response delete(Long var1);
}
