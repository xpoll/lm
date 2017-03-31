package cn.blmdz.hunt.engine.utils;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.google.common.base.Objects;
import com.google.common.base.Strings;

import cn.blmdz.hunt.engine.dao.RedisFileDao;

@Component
public class RedisFileLoader implements FileLoader {
   @Autowired
   private RedisFileDao redisFileDao;

   public FileLoader.Resp load(String path) {
      return this.load(path, (String)null);
   }

   public FileLoader.Resp load(String path, String sign) {
      Long updateAt = this.redisFileDao.getUpdateTime(path);
      if(updateAt == null) {
         return FileLoader.Resp.NOT_FOUND;
      } else {
         if(!Strings.isNullOrEmpty(sign)) {
            Long oldValue = Long.valueOf(sign);
            if(Objects.equal(oldValue, updateAt)) {
               return FileLoader.Resp.NOT_MODIFIED;
            }
         }

         String content = this.redisFileDao.getContent(path);
         FileLoader.Resp resp = new FileLoader.Resp();
         resp.setContextString(content);
         resp.setSign(updateAt.toString());
         return resp;
      }
   }
}
