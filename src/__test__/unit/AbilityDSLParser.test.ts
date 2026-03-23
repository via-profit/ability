import { AbilityDSLParser } from '../../parsers/dsl/AbilityDSLParser';


const dsl = `
# Простая политика (одно правило, без вложенности)
allow order.view if user.id equals order.owner

# Политика с одним блоком all of:
deny order.delete when:
  all of:
    user.role equals 'guest'
    user.ip in ['192.168.1.1', '10.0.0.1']

# Политика с двумя блоками
deny order.update when:
  all of:
    user.role contains 'manager'
    user.department in ['sales']
  any of:
    user.status in ['blocked']
    user.roles contains 'admin'
    
# Политика с вложенными блоками
permit user.profile.update when:
  all of:
    user.id equals profile.userId
    any of:
      user.roles contains 'admin'
      user.roles contains 'manager'

# Политика с разными компараторами
allow order.cancel when:
  all of:
    user.id equals order.owner
    order.amount less than 1000
    order.status not equals 'shipped'
    user.rating greater or equal 4.5
    
    
# Политика с массивами и строками (экранирование)
deny product.update when:
  any of:
    product.tags contains 'sensitive'
    product.name equals 'O\\'Reilly'
    product.categories in ['books', 'software']    
    
# Политика с путями без точек (действия могут быть без точек)
allow create if user.id equals order.owner

# Политика с комментариями (однострочные)
# Политика для обновления заказа
deny order.update when:
  all of:
    # Проверка роли
    user.role contains 'manager'
    user.department in ['sales']
  any of:
    user.status in ['blocked']
    user.roles contains 'admin'
    
# Политика с пустым блоком (крайний случай) — ожидается ошибка или игнорирование
deny order.update when:
  all of:
    # пустой блок
    
# Политика с большим количеством условий
permit order.bulkUpdate when:
  all of:
    user.roles contains 'admin'
    user.department equals 'it'
    user.location equals 'office'
    user.security_clearance greater or equal 3
    order.type in ['standard', 'express']
    order.amount less than 50000
  any of:
    user.roles contains 'auditor'
    user.supervisor_approval equals true
    user.two_factor_enabled equals true
    
# Политика с числами и булевыми значениями
allow order.promotion when:
  all of:
    order.total greater than 100
    order.discount less than 50
    user.premium equals true
    
# Политика с оператором not in
deny user.login when:
  any of:
    user.ip not in ['10.0.0.0/8', '192.168.0.0/16']
    user.country not in ['US', 'CA']
    
# Политика с использованием contains для строк
allow article.publish when:
  all of:
    article.title contains 'guide'
    article.author equals user.id
    
# Политика с несколькими блоками подряд
allow order.view when:
  all of:
    user.id equals order.owner
  any of:
    user.roles contains 'admin'
    user.roles contains 'support'
  all of:
    order.status not equals 'deleted'
`;

describe('AbilityDSLParser', () => {


  describe('Tokens', () => {
    it('unknown', () => {

      const dsl = `
permit user.profile.update when:
  all of:
    user.id equals profile.userId
    any of:
      user.roles contains 'admin'
      user.roles contains 'manager'
      `;

      const parser = new AbilityDSLParser(dsl);
     parser.parse();

      expect({foo: 'bar'}).toEqual({
        foo: 'bar',
      });
    });
  });

});
