package cn.blmdz.hunt.engine.utils;

import javax.annotation.PreDestroy;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;

import com.alibaba.dubbo.config.spring.ReferenceBean;
import com.alibaba.dubbo.config.spring.ServiceBean;
import com.google.common.base.Objects;
import com.google.common.base.Optional;
import com.google.common.base.Strings;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

public class DubboHelper {
   private static final Logger log = LoggerFactory.getLogger(DubboHelper.class);
   private static final String DEFAULT_VERSION = "DEFAULT";
   private final ApplicationContext applicationContext;
   private LoadingCache<DubboHelper.ReferenceKey, Optional> referenceCache;
   private LoadingCache<DubboHelper.ReferenceKey, ServiceBean> providerCache;

   @Autowired
   public DubboHelper(ApplicationContext applicationContext) {
      this.applicationContext = applicationContext;
   }

   public void init() {
      this.referenceCache = CacheBuilder.newBuilder().build(new CacheLoader<DubboHelper.ReferenceKey, Optional>() {
         public Optional load(DubboHelper.ReferenceKey key) throws Exception {
            ReferenceBean<Object> referenceBean = new ReferenceBean();
            referenceBean.setApplicationContext(DubboHelper.this.applicationContext);
            referenceBean.setInterface(key.clazz);
            if(!Strings.isNullOrEmpty(key.version) && !Objects.equal(key.version, "DEFAULT")) {
               referenceBean.setVersion(key.version);
            }

            if(!Strings.isNullOrEmpty(key.url)) {
               referenceBean.setUrl(key.url);
            }

            try {
               referenceBean.afterPropertiesSet();
               return Optional.of(new DubboHelper.ReferenceObj(referenceBean, referenceBean.get()));
            } catch (Exception var4) {
               DubboHelper.log.error("error when init dubbo reference bean. class {}, version {}", new Object[]{key.clazz, key.version, var4});
               return Optional.absent();
            }
         }
      });
      this.providerCache = CacheBuilder.newBuilder().build(new CacheLoader<DubboHelper.ReferenceKey, ServiceBean>() {
         public ServiceBean load(DubboHelper.ReferenceKey key) throws Exception {
            ServiceBean<Object> serviceBean = new ServiceBean();
            serviceBean.setApplicationContext(DubboHelper.this.applicationContext);
            serviceBean.setInterface(key.clazz);
            serviceBean.setRef(DubboHelper.this.applicationContext.getBean(key.clazz));
            if(!Strings.isNullOrEmpty(key.version) && !Objects.equal(key.version, "DEFAULT")) {
               serviceBean.setVersion(key.version);
            }

            serviceBean.afterPropertiesSet();
            return serviceBean;
         }
      });
   }

   @PreDestroy
   private void destroy() {
      for(Optional<DubboHelper.ReferenceObj> objOptional : this.referenceCache.asMap().values()) {
         if(objOptional.isPresent()) {
            ((DubboHelper.ReferenceObj)objOptional.get()).referenceBean.destroy();
         }
      }

      for(ServiceBean serviceBean : this.providerCache.asMap().values()) {
         try {
            serviceBean.destroy();
         } catch (Exception var4) {
            log.warn("error when destroy dubbo serviceBean", var4);
         }
      }

   }

   public Object getReference(Class clazz, String version) {
      return this.getReference(new DubboHelper.ReferenceKey(clazz, version));
   }

   public Object getReference(Class clazz, String version, String ip, Integer port) {
      port = (Integer)Objects.firstNonNull(port, Integer.valueOf(20880));
      String url = "dubbo://" + ip + ":" + port + "/" + clazz.getName();
      DubboHelper.ReferenceKey key = new DubboHelper.ReferenceKey(clazz, version);
      key.url = url;
      return this.getReference(key);
   }

   public void exportProvider(Class clazz, String version) {
      this.providerCache.getUnchecked(new DubboHelper.ReferenceKey(clazz, version));
   }

   private Object getReference(DubboHelper.ReferenceKey key) {
      Optional<DubboHelper.ReferenceObj> referenceOptional = (Optional)this.referenceCache.getUnchecked(key);
      return referenceOptional.isPresent()?((DubboHelper.ReferenceObj)referenceOptional.get()).bean:null;
   }

   private static class ReferenceKey {
      private Class clazz;
      private String version;
      private String url;

      private ReferenceKey(Class clazz, String version) {
         this.version = "DEFAULT";
         this.clazz = clazz;
         this.version = version;
      }

      public boolean equals(Object o) {
         if(this == o) {
            return true;
         } else if(!(o instanceof DubboHelper.ReferenceKey)) {
            return false;
         } else {
            DubboHelper.ReferenceKey that = (DubboHelper.ReferenceKey)o;
            if(!this.clazz.equals(that.clazz)) {
               return false;
            } else {
               if(this.url != null) {
                  if(!this.url.equals(that.url)) {
                     return false;
                  }
               } else if(that.url != null) {
                  return false;
               }

               if(!this.version.equals(that.version)) {
                  return false;
               } else {
                  return true;
               }
            }
         }
      }

      public int hashCode() {
         int result = this.clazz.hashCode();
         result = 31 * result + this.version.hashCode();
         result = 31 * result + (this.url != null?this.url.hashCode():0);
         return result;
      }
   }

   private static class ReferenceObj {
      private ReferenceBean referenceBean;
      private Object bean;

      private ReferenceObj(ReferenceBean referenceBean, Object bean) {
         this.referenceBean = referenceBean;
         this.bean = bean;
      }
   }
}
