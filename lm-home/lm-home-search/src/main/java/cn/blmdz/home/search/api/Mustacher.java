package cn.blmdz.home.search.api;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;
import com.google.common.base.Charsets;
import com.google.common.base.Throwables;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.io.Resources;

import cn.blmdz.home.search.SearchException;

@Component
public class Mustacher {
   private static final Logger log = LoggerFactory.getLogger(Mustacher.class);
   private final LoadingCache mustacheCache;
   private final MustacheFactory mf = new DefaultMustacheFactory();

   @Autowired
   public Mustacher(@Value("${search.template.cache: true}") boolean cache) {
      if(cache) {
         this.mustacheCache = CacheBuilder.newBuilder().build(new CacheLoader<String, Mustache>() {
            public Mustache load(String key) throws Exception {
               return Mustacher.this.compile(key);
            }
         });
      } else {
         this.mustacheCache = null;
      }

   }

   private Mustache compile(String key) throws IOException {
      String templateContent = Resources.toString(Resources.getResource(key), Charsets.UTF_8);
      Reader reader = new StringReader(templateContent);
      Throwable var4 = null;

      Mustache var5;
      try {
         var5 = this.mf.compile(reader, key);
      } catch (Throwable var14) {
         var4 = var14;
         throw var14;
      } finally {
         if(reader != null) {
            if(var4 != null) {
               try {
                  reader.close();
               } catch (Throwable var13) {
                  var4.addSuppressed(var13);
               }
            } else {
               reader.close();
            }
         }

      }

      return var5;
   }

   public Mustache forPath(String path) {
      try {
         return this.mustacheCache != null?(Mustache)this.mustacheCache.getUnchecked(path):this.compile(path);
      } catch (Exception var3) {
         log.error("failed to compile mustache template from {}, cause:{}", path, Throwables.getStackTraceAsString(var3));
         throw new SearchException(var3);
      }
   }
}
