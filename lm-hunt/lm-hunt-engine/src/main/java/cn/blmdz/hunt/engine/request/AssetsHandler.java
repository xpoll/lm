package cn.blmdz.hunt.engine.request;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import com.google.common.collect.Iterables;

import cn.blmdz.home.common.util.Splitters;
import cn.blmdz.hunt.engine.Setting;
import cn.blmdz.hunt.engine.utils.FileLoader;
import cn.blmdz.hunt.engine.utils.FileLoaderHelper;
import cn.blmdz.hunt.engine.utils.MimeTypes;

@Component
public class AssetsHandler {
   private static final Logger log = LoggerFactory.getLogger(AssetsHandler.class);
   @Autowired
   private Setting setting;
   @Autowired
   private FileLoaderHelper fileLoaderHelper;

   public boolean handle(String path, HttpServletResponse response) {
      String lastPath = (String)Iterables.getLast(Splitters.SLASH.split(path));
      List<String> fileInfo = Splitters.DOT.splitToList(lastPath);
      if(fileInfo.size() == 1) {
         return false;
      } else {
         response.setContentType(MimeTypes.getType((String)Iterables.getLast(fileInfo)));
         String realPath = this.setting.getRootPath() + path;
         FileLoader.Resp resp = this.fileLoaderHelper.load(realPath);
         if(resp.isNotFound()) {
            if(log.isDebugEnabled()) {
               log.debug("Assets not found, path: [{}]", path);
            }

            response.setStatus(HttpStatus.NOT_FOUND.value());
            return true;
         } else {
            response.setContentLength(resp.getContext().length);

            try {
               response.getOutputStream().write(resp.getContext());
            } catch (IOException var8) {
               ;
            }

            return true;
         }
      }
   }
}
