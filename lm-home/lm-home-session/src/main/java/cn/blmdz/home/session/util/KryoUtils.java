package cn.blmdz.home.session.util;

import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.Serializer;
import java.util.ArrayList;
import java.util.List;

public class KryoUtils {
   private static final List classList = new ArrayList();
   private static final List serializerList = new ArrayList();
   private static final List idList = new ArrayList();
   private static final ThreadLocal kryos = new ThreadLocal() {
      protected Kryo initialValue() {
         Kryo kryo = new Kryo();
         int size = KryoUtils.idList.size();

         for(int i = 0; i < size; ++i) {
            kryo.register((Class)KryoUtils.classList.get(i), (Serializer)KryoUtils.serializerList.get(i), ((Integer)KryoUtils.idList.get(i)).intValue());
         }

         kryo.setReferences(false);
         return kryo;
      }
   };

   public static synchronized void registerClass(Class className, Serializer serializer, int id) {
      classList.add(className);
      serializerList.add(serializer);
      idList.add(Integer.valueOf(id));
   }

   public static Kryo getKryo() {
      return (Kryo)kryos.get();
   }
}
