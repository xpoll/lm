package cn.blmdz.boot.mybatis.autoconfigure;

import cn.blmdz.boot.mybatis.autoconfigure.MybatisProperties;

import com.google.common.base.Splitter;
import com.google.common.collect.Iterators;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import javax.sql.DataSource;

import org.apache.ibatis.session.SqlSessionFactory;
import org.mybatis.spring.SqlSessionFactoryBean;
import org.mybatis.spring.SqlSessionTemplate;
import org.reflections.Reflections;
import org.reflections.scanners.ResourcesScanner;
import org.reflections.scanners.Scanner;
import org.reflections.scanners.SubTypesScanner;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;
import org.reflections.util.FilterBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnClass({SqlSessionFactory.class, SqlSessionFactoryBean.class})
@ConditionalOnMissingBean({SqlSessionTemplate.class})
@ConditionalOnBean({DataSource.class})
@EnableConfigurationProperties({MybatisProperties.class})
@AutoConfigureAfter({DataSourceAutoConfiguration.class})
public class MybatisAutoConfiguration {
   @Autowired
   private MybatisProperties properties;

   @Bean
   public SqlSessionFactory sqlSessionFactory(DataSource dataSource) throws Exception {
      SqlSessionFactoryBean sqlSessionFactoryBean = new SqlSessionFactoryBean();
      sqlSessionFactoryBean.setDataSource(dataSource);
      String path = this.properties.getTypeAliasesPackage();
      List<String> packageNames = Splitter.on(",").trimResults().omitEmptyStrings().splitToList(path);
      List<Class> classes = new ArrayList();

      for(String packageName : packageNames) {
         classes.addAll(this.resolveAlias(packageName, Object.class));
      }

      sqlSessionFactoryBean.setTypeAliases((Class[])Iterators.toArray(classes.iterator(), Class.class));
      sqlSessionFactoryBean.setMapperLocations(this.properties.resolveMapperLocations());
      return sqlSessionFactoryBean.getObject();
   }

   public List resolveAlias(String packageName, Class superType) {
      List<ClassLoader> classLoadersList = new LinkedList();
      classLoadersList.add(ClasspathHelper.contextClassLoader());
      classLoadersList.add(ClasspathHelper.staticClassLoader());
      Reflections reflections = new Reflections((new ConfigurationBuilder()).setScanners(new Scanner[]{new SubTypesScanner(false), new ResourcesScanner()}).setUrls(ClasspathHelper.forClassLoader((ClassLoader[])classLoadersList.toArray(new ClassLoader[0]))).filterInputsBy((new FilterBuilder()).include(FilterBuilder.prefix(packageName))));
      Set classes = reflections.getSubTypesOf(superType);
      List<Class> classList = new ArrayList();

      for(Object c : classes) {
         if(!((Class)c).isAnonymousClass() && !((Class)c).isInterface() && !((Class)c).isMemberClass()) {
            classList.add((Class)c);
         }
      }

      return classList;
   }

   @Bean
   public SqlSessionTemplate sqlSessionTemplate(SqlSessionFactory sqlSessionFactory) {
      return new SqlSessionTemplate(sqlSessionFactory, this.properties.getExecutorType());
   }
}
